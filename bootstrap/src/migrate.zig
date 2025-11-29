const std = @import("std");
const lexer_module = @import("lexer.zig");
const parser = @import("parser.zig");
const linearity_inference = @import("linearity_inference.zig");

/// Auto-migration tool to remove unnecessary explicit annotations
/// Runs linearity inference and suggests removing annotations that match inferred values
pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    if (args.len < 2) {
        std.debug.print("Usage: migrate <source_file>\n", .{});
        return;
    }

    const source_path = args[1];
    const source_code = try std.fs.cwd().readFileAlloc(allocator, source_path, 1024 * 1024);
    defer allocator.free(source_code);

    // Tokenize
    var lex = lexer_module.Lexer.init(source_code);
    const tokens = try lex.tokenize(allocator);

    // Parse
    var p = parser.Parser.init(tokens.items, allocator);

    var ast = p.parse() catch |err| {
        std.debug.print("Parse error: {}\n", .{err});
        return;
    };
    defer ast.deinit(allocator);

    // Run linearity inference
    var inference_engine = linearity_inference.LinearityInferenceEngine.init(allocator);
    defer inference_engine.deinit();

    try inference_engine.inferAll(&ast);

    // Output inferred annotations for all functions
    std.debug.print("\n=== Inferred Annotations ===\n\n", .{});
    for (ast.functions.items) |func| {
        std.debug.print("fn {s}(", .{func.name});
        for (func.params, 0..) |param, i| {
            if (i > 0) std.debug.print(", ", .{});
            std.debug.print("{s}: ", .{param.name});

            if (inference_engine.getInferredAnnotation(param.name)) |inferred| {
                // Show type and inferred annotation
                std.debug.print("...", .{});
                printAnnotation(inferred);
            } else {
                std.debug.print("???", .{});
            }
        }
        std.debug.print(")\n", .{});
    }

    // Analyze annotations and suggest removals
    std.debug.print("\n=== Removable Annotations ===\n\n", .{});
    var removable_count: usize = 0;
    for (ast.functions.items) |func| {
        for (func.params) |param| {
            if (param.param_type.annotation_source == .explicit) {
                const explicit = param.param_type.usage;
                if (inference_engine.getInferredAnnotation(param.name)) |inferred| {
                    // Check if annotation matches inferred value
                    if (annotationsMatch(explicit, inferred)) {
                        std.debug.print("removable: function '{s}' parameter '{s}' has explicit annotation ", .{ func.name, param.name });
                        printAnnotation(explicit);
                        std.debug.print(" which matches inferred value\n", .{});
                        removable_count += 1;
                    }
                }
            }
        }
    }

    if (removable_count == 0) {
        std.debug.print("(none)\n", .{});
    }
}

fn annotationsMatch(explicit: parser.UsageAnnotation, inferred: parser.UsageAnnotation) bool {
    return switch (explicit) {
        .once => switch (inferred) {
            .once => true,
            else => false,
        },
        .optional => switch (inferred) {
            .optional => true,
            else => false,
        },
        .unlimited => switch (inferred) {
            .unlimited => true,
            else => false,
        },
        .exactly => |explicit_n| switch (inferred) {
            .exactly => |inferred_n| explicit_n == inferred_n,
            else => false,
        },
    };
}

fn printAnnotation(annotation: parser.UsageAnnotation) void {
    switch (annotation) {
        .once => std.debug.print("@1", .{}),
        .optional => std.debug.print("@?", .{}),
        .unlimited => std.debug.print("@*", .{}),
        .exactly => |n| std.debug.print("@{d}", .{n}),
    }
}
