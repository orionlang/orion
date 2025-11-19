//! By convention, root.zig is the root source file when making a library.
const std = @import("std");

test {
    _ = @import("lexer.zig");
    _ = @import("parser.zig");
    _ = @import("typechecker.zig");
    _ = @import("codegen.zig");
    _ = @import("integration_test.zig");
}
