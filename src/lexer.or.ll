; Bootstrap Orion Compiler Output
target triple = "x86_64-pc-linux-gnu"

declare ptr @mmap(ptr, i64, i32, i32, i32, i64)
declare i32 @munmap(ptr, i64)

; Coroutine intrinsics for structured concurrency
declare token @llvm.coro.id(i32, ptr, ptr, ptr)
declare i64 @llvm.coro.size.i64()
declare ptr @llvm.coro.begin(token, ptr)
declare token @llvm.coro.save(ptr)
declare i8 @llvm.coro.suspend(token, i1)
declare i1 @llvm.coro.end(ptr, i1, token)
declare ptr @llvm.coro.free(token, ptr)
declare void @llvm.coro.resume(ptr)
declare void @llvm.coro.destroy(ptr)
declare ptr @llvm.coro.promise(ptr, i32, i1)
declare i1 @llvm.coro.done(ptr)
; Memory allocation for coroutine frames
declare ptr @malloc(i64)
declare void @free(ptr)

define {  } @memcpy(ptr %dest, ptr %src, i64 %size) {
entry:
  %dest.addr = alloca ptr
  store ptr %dest, ptr %dest.addr
  %src.addr = alloca ptr
  store ptr %src, ptr %src.addr
  %size.addr = alloca i64
  store i64 %size, ptr %size.addr
  %i = alloca i64
  store i64 0, ptr %i
  br label %while_header0
while_header0:
  %.t3 = load i64, ptr %i
  %.t4 = load i64, ptr %size.addr
  %.t5 = icmp ult i64 %.t3, %.t4
  br i1 %.t5, label %while_body1, label %while_exit2
while_body1:
  %.t6 = load ptr, ptr %src.addr
  %.t7 = load i64, ptr %i
  %.t8 = getelementptr i64, ptr %.t6, i64 %.t7
  %src_byte_ptr = alloca ptr
  store ptr %.t8, ptr %src_byte_ptr
  %.t9 = load ptr, ptr %src_byte_ptr
  %.t10 = load i8, ptr %.t9
  %.t11 = zext i8 %.t10 to i64
  %.t12 = trunc i64 %.t11 to i8
  %byte = alloca i8
  store i8 %.t12, ptr %byte
  %.t13 = load ptr, ptr %dest.addr
  %.t14 = load i64, ptr %i
  %.t15 = getelementptr i64, ptr %.t13, i64 %.t14
  %dest_byte_ptr = alloca ptr
  store ptr %.t15, ptr %dest_byte_ptr
  %.t16 = load ptr, ptr %dest_byte_ptr
  %.t17 = load i8, ptr %byte
  store i8 %.t17, ptr %.t16
  %.t18 = load i64, ptr %i
  %.t19 = add i64 %.t18, 1
  store i64 %.t19, ptr %i
  br label %while_header0
while_exit2:
  ret {  } undef
}

define i1 @is_alpha(i8 %c) {
entry:
  %c.addr = alloca i8
  store i8 %c, ptr %c.addr
  %.t20 = load i8, ptr %c.addr
  %.t21 = icmp uge i8 %.t20, 97
  %.t22 = load i8, ptr %c.addr
  %.t23 = icmp ule i8 %.t22, 122
  %.t24 = and i1 %.t21, %.t23
  %lower = alloca i1
  store i1 %.t24, ptr %lower
  %.t25 = load i8, ptr %c.addr
  %.t26 = icmp uge i8 %.t25, 65
  %.t27 = load i8, ptr %c.addr
  %.t28 = icmp ule i8 %.t27, 90
  %.t29 = and i1 %.t26, %.t28
  %upper = alloca i1
  store i1 %.t29, ptr %upper
  %.t30 = load i1, ptr %lower
  %.t31 = load i1, ptr %upper
  %.t32 = or i1 %.t30, %.t31
  ret i1 %.t32
}

define i1 @is_digit(i8 %c) {
entry:
  %c.addr = alloca i8
  store i8 %c, ptr %c.addr
  %.t33 = load i8, ptr %c.addr
  %.t34 = icmp uge i8 %.t33, 48
  %.t35 = load i8, ptr %c.addr
  %.t36 = icmp ule i8 %.t35, 57
  %.t37 = and i1 %.t34, %.t36
  ret i1 %.t37
}

define i1 @is_alphanumeric(i8 %c) {
entry:
  %c.addr = alloca i8
  store i8 %c, ptr %c.addr
  %.t38 = load i8, ptr %c.addr
  %.t39 = call i1 @is_alpha(i8 %.t38)
  %.t40 = load i8, ptr %c.addr
  %.t41 = call i1 @is_digit(i8 %.t40)
  %.t42 = or i1 %.t39, %.t41
  ret i1 %.t42
}

define i1 @is_whitespace(i8 %c) {
entry:
  %c.addr = alloca i8
  store i8 %c, ptr %c.addr
  %.t43 = load i8, ptr %c.addr
  %.t44 = icmp eq i8 %.t43, 32
  %space = alloca i1
  store i1 %.t44, ptr %space
  %.t45 = load i8, ptr %c.addr
  %.t46 = icmp eq i8 %.t45, 9
  %tab = alloca i1
  store i1 %.t46, ptr %tab
  %.t47 = load i8, ptr %c.addr
  %.t48 = icmp eq i8 %.t47, 10
  %newline = alloca i1
  store i1 %.t48, ptr %newline
  %.t49 = load i8, ptr %c.addr
  %.t50 = icmp eq i8 %.t49, 13
  %carriage = alloca i1
  store i1 %.t50, ptr %carriage
  %.t51 = load i1, ptr %space
  %.t52 = load i1, ptr %tab
  %.t53 = or i1 %.t51, %.t52
  %.t54 = load i1, ptr %newline
  %.t55 = or i1 %.t53, %.t54
  %.t56 = load i1, ptr %carriage
  %.t57 = or i1 %.t55, %.t56
  ret i1 %.t57
}

define i1 @is_hex_digit(i8 %c) {
entry:
  %c.addr = alloca i8
  store i8 %c, ptr %c.addr
  %.t58 = load i8, ptr %c.addr
  %.t59 = call i1 @is_digit(i8 %.t58)
  %digit = alloca i1
  store i1 %.t59, ptr %digit
  %.t60 = load i8, ptr %c.addr
  %.t61 = icmp uge i8 %.t60, 97
  %.t62 = load i8, ptr %c.addr
  %.t63 = icmp ule i8 %.t62, 102
  %.t64 = and i1 %.t61, %.t63
  %lower_hex = alloca i1
  store i1 %.t64, ptr %lower_hex
  %.t65 = load i8, ptr %c.addr
  %.t66 = icmp uge i8 %.t65, 65
  %.t67 = load i8, ptr %c.addr
  %.t68 = icmp ule i8 %.t67, 70
  %.t69 = and i1 %.t66, %.t68
  %upper_hex = alloca i1
  store i1 %.t69, ptr %upper_hex
  %.t70 = load i1, ptr %digit
  %.t71 = load i1, ptr %lower_hex
  %.t72 = or i1 %.t70, %.t71
  %.t73 = load i1, ptr %upper_hex
  %.t74 = or i1 %.t72, %.t73
  ret i1 %.t74
}

define i1 @is_octal_digit(i8 %c) {
entry:
  %c.addr = alloca i8
  store i8 %c, ptr %c.addr
  %.t75 = load i8, ptr %c.addr
  %.t76 = icmp uge i8 %.t75, 48
  %.t77 = load i8, ptr %c.addr
  %.t78 = icmp ule i8 %.t77, 55
  %.t79 = and i1 %.t76, %.t78
  ret i1 %.t79
}

define i1 @is_binary_digit(i8 %c) {
entry:
  %c.addr = alloca i8
  store i8 %c, ptr %c.addr
  %.t80 = load i8, ptr %c.addr
  %.t81 = icmp eq i8 %.t80, 48
  %.t82 = load i8, ptr %c.addr
  %.t83 = icmp eq i8 %.t82, 49
  %.t84 = or i1 %.t81, %.t83
  ret i1 %.t84
}

define i1 @is_punctuation(i8 %c) {
entry:
  %c.addr = alloca i8
  store i8 %c, ptr %c.addr
  %.t85 = load i8, ptr %c.addr
  %.t86 = icmp uge i8 %.t85, 33
  %.t87 = load i8, ptr %c.addr
  %.t88 = icmp ule i8 %.t87, 47
  %.t89 = and i1 %.t86, %.t88
  %range1 = alloca i1
  store i1 %.t89, ptr %range1
  %.t90 = load i8, ptr %c.addr
  %.t91 = icmp uge i8 %.t90, 58
  %.t92 = load i8, ptr %c.addr
  %.t93 = icmp ule i8 %.t92, 64
  %.t94 = and i1 %.t91, %.t93
  %range2 = alloca i1
  store i1 %.t94, ptr %range2
  %.t95 = load i8, ptr %c.addr
  %.t96 = icmp uge i8 %.t95, 91
  %.t97 = load i8, ptr %c.addr
  %.t98 = icmp ule i8 %.t97, 96
  %.t99 = and i1 %.t96, %.t98
  %range3 = alloca i1
  store i1 %.t99, ptr %range3
  %.t100 = load i8, ptr %c.addr
  %.t101 = icmp uge i8 %.t100, 123
  %.t102 = load i8, ptr %c.addr
  %.t103 = icmp ule i8 %.t102, 126
  %.t104 = and i1 %.t101, %.t103
  %range4 = alloca i1
  store i1 %.t104, ptr %range4
  %.t105 = load i1, ptr %range1
  %.t106 = load i1, ptr %range2
  %.t107 = or i1 %.t105, %.t106
  %.t108 = load i1, ptr %range3
  %.t109 = or i1 %.t107, %.t108
  %.t110 = load i1, ptr %range4
  %.t111 = or i1 %.t109, %.t110
  ret i1 %.t111
}

define i1 @is_printable(i8 %c) {
entry:
  %c.addr = alloca i8
  store i8 %c, ptr %c.addr
  %.t112 = load i8, ptr %c.addr
  %.t113 = icmp uge i8 %.t112, 32
  %.t114 = load i8, ptr %c.addr
  %.t115 = icmp ule i8 %.t114, 126
  %.t116 = and i1 %.t113, %.t115
  ret i1 %.t116
}

define i1 @is_control(i8 %c) {
entry:
  %c.addr = alloca i8
  store i8 %c, ptr %c.addr
  %.t117 = load i8, ptr %c.addr
  %.t118 = icmp ult i8 %.t117, 32
  %.t119 = load i8, ptr %c.addr
  %.t120 = icmp eq i8 %.t119, 127
  %.t121 = or i1 %.t118, %.t120
  ret i1 %.t121
}

define i1 @is_upper(i8 %c) {
entry:
  %c.addr = alloca i8
  store i8 %c, ptr %c.addr
  %.t122 = load i8, ptr %c.addr
  %.t123 = icmp uge i8 %.t122, 65
  %.t124 = load i8, ptr %c.addr
  %.t125 = icmp ule i8 %.t124, 90
  %.t126 = and i1 %.t123, %.t125
  ret i1 %.t126
}

define i1 @is_lower(i8 %c) {
entry:
  %c.addr = alloca i8
  store i8 %c, ptr %c.addr
  %.t127 = load i8, ptr %c.addr
  %.t128 = icmp uge i8 %.t127, 97
  %.t129 = load i8, ptr %c.addr
  %.t130 = icmp ule i8 %.t129, 122
  %.t131 = and i1 %.t128, %.t130
  ret i1 %.t131
}

define i8 @to_lower(i8 %c) {
entry:
  %c.addr = alloca i8
  store i8 %c, ptr %c.addr
  %.t132 = load i8, ptr %c.addr
  %.t133 = call i1 @is_upper(i8 %.t132)
  br i1 %.t133, label %then134, label %else135
then134:
  %.t137 = load i8, ptr %c.addr
  %.t138 = add i8 %.t137, 32
  br label %merge136
else135:
  %.t139 = load i8, ptr %c.addr
  br label %merge136
merge136:
  %.t140 = phi i8 [ %.t138, %then134 ], [ %.t139, %else135 ]
  ret i8 %.t140
}

define i8 @to_upper(i8 %c) {
entry:
  %c.addr = alloca i8
  store i8 %c, ptr %c.addr
  %.t141 = load i8, ptr %c.addr
  %.t142 = call i1 @is_lower(i8 %.t141)
  br i1 %.t142, label %then143, label %else144
then143:
  %.t146 = load i8, ptr %c.addr
  %.t147 = sub i8 %.t146, 32
  br label %merge145
else144:
  %.t148 = load i8, ptr %c.addr
  br label %merge145
merge145:
  %.t149 = phi i8 [ %.t147, %then143 ], [ %.t148, %else144 ]
  ret i8 %.t149
}

define i8 @hex_to_int(i8 %c) {
entry:
  %c.addr = alloca i8
  store i8 %c, ptr %c.addr
  %.t150 = load i8, ptr %c.addr
  %.t151 = call i1 @is_digit(i8 %.t150)
  br i1 %.t151, label %then152, label %else153
then152:
  %.t155 = load i8, ptr %c.addr
  %.t156 = sub i8 %.t155, 48
  br label %merge154
else153:
  %.t157 = load i8, ptr %c.addr
  %.t158 = icmp uge i8 %.t157, 97
  %.t159 = load i8, ptr %c.addr
  %.t160 = icmp ule i8 %.t159, 102
  %.t161 = and i1 %.t158, %.t160
  br i1 %.t161, label %then162, label %else163
then162:
  %.t165 = load i8, ptr %c.addr
  %.t166 = sub i8 %.t165, 97
  %.t167 = add i8 %.t166, 10
  br label %merge164
else163:
  %.t168 = load i8, ptr %c.addr
  %.t169 = icmp uge i8 %.t168, 65
  %.t170 = load i8, ptr %c.addr
  %.t171 = icmp ule i8 %.t170, 70
  %.t172 = and i1 %.t169, %.t171
  br i1 %.t172, label %then173, label %else174
then173:
  %.t176 = load i8, ptr %c.addr
  %.t177 = sub i8 %.t176, 65
  %.t178 = add i8 %.t177, 10
  br label %merge175
else174:
  br label %merge175
merge175:
  %.t179 = phi i8 [ %.t178, %then173 ], [ 0, %else174 ]
  br label %merge164
merge164:
  %.t180 = phi i8 [ %.t167, %then162 ], [ %.t179, %merge175 ]
  br label %merge154
merge154:
  %.t181 = phi i8 [ %.t156, %then152 ], [ %.t180, %merge164 ]
  ret i8 %.t181
}

define i8 @digit_to_int(i8 %c) {
entry:
  %c.addr = alloca i8
  store i8 %c, ptr %c.addr
  %.t182 = load i8, ptr %c.addr
  %.t183 = call i1 @is_digit(i8 %.t182)
  br i1 %.t183, label %then184, label %else185
then184:
  %.t187 = load i8, ptr %c.addr
  %.t188 = sub i8 %.t187, 48
  br label %merge186
else185:
  br label %merge186
merge186:
  %.t189 = phi i8 [ %.t188, %then184 ], [ 0, %else185 ]
  ret i8 %.t189
}

define i1 @is_ident_start(i8 %c) {
entry:
  %c.addr = alloca i8
  store i8 %c, ptr %c.addr
  %.t190 = load i8, ptr %c.addr
  %.t191 = call i1 @is_alpha(i8 %.t190)
  %.t192 = load i8, ptr %c.addr
  %.t193 = icmp eq i8 %.t192, 95
  %.t194 = or i1 %.t191, %.t193
  ret i1 %.t194
}

define i1 @is_ident_continue(i8 %c) {
entry:
  %c.addr = alloca i8
  store i8 %c, ptr %c.addr
  %.t195 = load i8, ptr %c.addr
  %.t196 = call i1 @is_alphanumeric(i8 %.t195)
  %.t197 = load i8, ptr %c.addr
  %.t198 = icmp eq i8 %.t197, 95
  %.t199 = or i1 %.t196, %.t198
  ret i1 %.t199
}

define { { i64 }, ptr, i64, i64, i64 } @arena_create({ i64 } %parent_alloc) {
entry:
  %parent_alloc.addr = alloca { i64 }
  store { i64 } %parent_alloc, ptr %parent_alloc.addr
  %null_addr = alloca i64
  store i64 0, ptr %null_addr
  %.t200 = load i64, ptr %null_addr
  %.t201 = inttoptr i64 %.t200 to ptr
  %null_ptr = alloca ptr
  store ptr %.t201, ptr %null_ptr
  %.t202 = load { i64 }, ptr %parent_alloc.addr
  %.t203 = insertvalue { { i64 }, ptr, i64, i64, i64 } undef, { i64 } %.t202, 0
  %.t204 = load ptr, ptr %null_ptr
  %.t205 = insertvalue { { i64 }, ptr, i64, i64, i64 } %.t203, ptr %.t204, 1
  %.t206 = insertvalue { { i64 }, ptr, i64, i64, i64 } %.t205, i64 0, 2
  %.t207 = insertvalue { { i64 }, ptr, i64, i64, i64 } %.t206, i64 0, 3
  %.t208 = insertvalue { { i64 }, ptr, i64, i64, i64 } %.t207, i64 0, 4
  ret { { i64 }, ptr, i64, i64, i64 } %.t208
}

define { { i64 }, ptr, i64, i64, i64 } @arena_reset({ { i64 }, ptr, i64, i64, i64 } %arena) {
entry:
  %arena.addr = alloca { { i64 }, ptr, i64, i64, i64 }
  store { { i64 }, ptr, i64, i64, i64 } %arena, ptr %arena.addr
  %.t209 = load { { i64 }, ptr, i64, i64, i64 }, ptr %arena.addr
  %.t210 = extractvalue { { i64 }, ptr, i64, i64, i64 } %.t209, 0
  %.t211 = insertvalue { { i64 }, ptr, i64, i64, i64 } undef, { i64 } %.t210, 0
  %.t212 = load { { i64 }, ptr, i64, i64, i64 }, ptr %arena.addr
  %.t213 = extractvalue { { i64 }, ptr, i64, i64, i64 } %.t212, 1
  %.t214 = insertvalue { { i64 }, ptr, i64, i64, i64 } %.t211, ptr %.t213, 1
  %.t215 = load { { i64 }, ptr, i64, i64, i64 }, ptr %arena.addr
  %.t216 = extractvalue { { i64 }, ptr, i64, i64, i64 } %.t215, 2
  %.t217 = insertvalue { { i64 }, ptr, i64, i64, i64 } %.t214, i64 %.t216, 2
  %.t218 = insertvalue { { i64 }, ptr, i64, i64, i64 } %.t217, i64 0, 3
  %.t219 = load { { i64 }, ptr, i64, i64, i64 }, ptr %arena.addr
  %.t220 = extractvalue { { i64 }, ptr, i64, i64, i64 } %.t219, 4
  %.t221 = insertvalue { { i64 }, ptr, i64, i64, i64 } %.t218, i64 %.t220, 4
  ret { { i64 }, ptr, i64, i64, i64 } %.t221
}

define {  } @arena_destroy({ { i64 }, ptr, i64, i64, i64 } %arena) {
entry:
  %arena.addr = alloca { { i64 }, ptr, i64, i64, i64 }
  store { { i64 }, ptr, i64, i64, i64 } %arena, ptr %arena.addr
  %.t222 = load { { i64 }, ptr, i64, i64, i64 }, ptr %arena.addr
  %.t223 = extractvalue { { i64 }, ptr, i64, i64, i64 } %.t222, 4
  %.t224 = icmp ugt i64 %.t223, 0
  %has_buffer = alloca i1
  store i1 %.t224, ptr %has_buffer
  %.t228 = load i1, ptr %has_buffer
  br i1 %.t228, label %if_then225, label %if_end227
if_then225:
  %.t229 = load { { i64 }, ptr, i64, i64, i64 }, ptr %arena.addr
  %.t230 = extractvalue { { i64 }, ptr, i64, i64, i64 } %.t229, 0
  %.t231 = load { { i64 }, ptr, i64, i64, i64 }, ptr %arena.addr
  %.t232 = extractvalue { { i64 }, ptr, i64, i64, i64 } %.t231, 1
  %.t233 = load { { i64 }, ptr, i64, i64, i64 }, ptr %arena.addr
  %.t234 = extractvalue { { i64 }, ptr, i64, i64, i64 } %.t233, 4
  %.t235 = call {  } @PageAllocator__free({ i64 } %.t230, ptr %.t232, i64 %.t234)
  br label %if_end227
if_end227:
  ret {  } undef
}

define { i64 } @page_allocator_create() {
entry:
  %.t236 = insertvalue { i64 } undef, i64 4096, 0
  ret { i64 } %.t236
}

define { { i64 }, ptr, ptr, ptr } @gpa_create({ i64 } %parent_alloc) {
entry:
  %parent_alloc.addr = alloca { i64 }
  store { i64 } %parent_alloc, ptr %parent_alloc.addr
  %null_addr = alloca i64
  store i64 0, ptr %null_addr
  %.t237 = load i64, ptr %null_addr
  %.t238 = inttoptr i64 %.t237 to ptr
  %null_ptr = alloca ptr
  store ptr %.t238, ptr %null_ptr
  %.t239 = load { i64 }, ptr %parent_alloc.addr
  %.t240 = insertvalue { { i64 }, ptr, ptr, ptr } undef, { i64 } %.t239, 0
  %.t241 = load ptr, ptr %null_ptr
  %.t242 = insertvalue { { i64 }, ptr, ptr, ptr } %.t240, ptr %.t241, 1
  %.t243 = load ptr, ptr %null_ptr
  %.t244 = insertvalue { { i64 }, ptr, ptr, ptr } %.t242, ptr %.t243, 2
  %.t245 = load ptr, ptr %null_ptr
  %.t246 = insertvalue { { i64 }, ptr, ptr, ptr } %.t244, ptr %.t245, 3
  ret { { i64 }, ptr, ptr, ptr } %.t246
}

define { ptr, i64, i64, i64 } @new_lexer(ptr %source) {
entry:
  %source.addr = alloca ptr
  store ptr %source, ptr %source.addr
  %.t247 = load ptr, ptr %source.addr
  %.t248 = insertvalue { ptr, i64, i64, i64 } undef, ptr %.t247, 0
  %.t249 = insertvalue { ptr, i64, i64, i64 } %.t248, i64 0, 1
  %.t250 = insertvalue { ptr, i64, i64, i64 } %.t249, i64 1, 2
  %.t251 = insertvalue { ptr, i64, i64, i64 } %.t250, i64 0, 3
  ret { ptr, i64, i64, i64 } %.t251
}

define i8 @peek({ ptr, i64, i64, i64 } %lexer) {
entry:
  %lexer.addr = alloca { ptr, i64, i64, i64 }
  store { ptr, i64, i64, i64 } %lexer, ptr %lexer.addr
  %.t255 = load { ptr, i64, i64, i64 }, ptr %lexer.addr
  %.t256 = extractvalue { ptr, i64, i64, i64 } %.t255, 1
  %.t257 = load { ptr, i64, i64, i64 }, ptr %lexer.addr
  %.t258 = extractvalue { ptr, i64, i64, i64 } %.t257, 0
  %.t259 = call i64 @str__len(ptr %.t258)
  %.t260 = icmp slt i64 %.t256, %.t259
  br i1 %.t260, label %if_then252, label %if_end254
if_then252:
  %.t261 = load { ptr, i64, i64, i64 }, ptr %lexer.addr
  %.t262 = extractvalue { ptr, i64, i64, i64 } %.t261, 0
  %.t263 = load { ptr, i64, i64, i64 }, ptr %lexer.addr
  %.t264 = extractvalue { ptr, i64, i64, i64 } %.t263, 1
  %.t265 = call i8 @str__char_at(ptr %.t262, i64 %.t264)
  ret i8 %.t265
  br label %if_end254
if_end254:
  ret i8 255
}

define i8 @peek_ahead({ ptr, i64, i64, i64 } %lexer, i64 %n) {
entry:
  %lexer.addr = alloca { ptr, i64, i64, i64 }
  store { ptr, i64, i64, i64 } %lexer, ptr %lexer.addr
  %n.addr = alloca i64
  store i64 %n, ptr %n.addr
  %.t269 = load { ptr, i64, i64, i64 }, ptr %lexer.addr
  %.t270 = extractvalue { ptr, i64, i64, i64 } %.t269, 1
  %.t271 = load i64, ptr %n.addr
  %.t272 = add i64 %.t270, %.t271
  %.t273 = load { ptr, i64, i64, i64 }, ptr %lexer.addr
  %.t274 = extractvalue { ptr, i64, i64, i64 } %.t273, 0
  %.t275 = call i64 @str__len(ptr %.t274)
  %.t276 = icmp slt i64 %.t272, %.t275
  br i1 %.t276, label %if_then266, label %if_end268
if_then266:
  %.t277 = load { ptr, i64, i64, i64 }, ptr %lexer.addr
  %.t278 = extractvalue { ptr, i64, i64, i64 } %.t277, 0
  %.t279 = load { ptr, i64, i64, i64 }, ptr %lexer.addr
  %.t280 = extractvalue { ptr, i64, i64, i64 } %.t279, 1
  %.t281 = load i64, ptr %n.addr
  %.t282 = add i64 %.t280, %.t281
  %.t283 = call i8 @str__char_at(ptr %.t278, i64 %.t282)
  ret i8 %.t283
  br label %if_end268
if_end268:
  ret i8 255
}

define { ptr, i64, i64, i64 } @advance({ ptr, i64, i64, i64 } %lexer) {
entry:
  %lexer.addr = alloca { ptr, i64, i64, i64 }
  store { ptr, i64, i64, i64 } %lexer, ptr %lexer.addr
  %.t284 = load { ptr, i64, i64, i64 }, ptr %lexer.addr
  %.t285 = call i8 @peek({ ptr, i64, i64, i64 } %.t284)
  %c = alloca i8
  store i8 %.t285, ptr %c
  %.t289 = load i8, ptr %c
  %.t290 = icmp eq i8 %.t289, 255
  br i1 %.t290, label %if_then286, label %if_end288
if_then286:
  %.t291 = load { ptr, i64, i64, i64 }, ptr %lexer.addr
  ret { ptr, i64, i64, i64 } %.t291
  br label %if_end288
if_end288:
  %.t295 = load i8, ptr %c
  %.t296 = icmp eq i8 %.t295, 10
  br i1 %.t296, label %if_then292, label %if_else293
if_then292:
  %.t297 = load { ptr, i64, i64, i64 }, ptr %lexer.addr
  %.t298 = extractvalue { ptr, i64, i64, i64 } %.t297, 0
  %.t299 = insertvalue { ptr, i64, i64, i64 } undef, ptr %.t298, 0
  %.t300 = load { ptr, i64, i64, i64 }, ptr %lexer.addr
  %.t301 = extractvalue { ptr, i64, i64, i64 } %.t300, 1
  %.t302 = add i64 %.t301, 1
  %.t303 = insertvalue { ptr, i64, i64, i64 } %.t299, i64 %.t302, 1
  %.t304 = load { ptr, i64, i64, i64 }, ptr %lexer.addr
  %.t305 = extractvalue { ptr, i64, i64, i64 } %.t304, 2
  %.t306 = add i64 %.t305, 1
  %.t307 = insertvalue { ptr, i64, i64, i64 } %.t303, i64 %.t306, 2
  %.t308 = insertvalue { ptr, i64, i64, i64 } %.t307, i64 0, 3
  ret { ptr, i64, i64, i64 } %.t308
  br label %if_end294
if_else293:
  %.t309 = load { ptr, i64, i64, i64 }, ptr %lexer.addr
  %.t310 = extractvalue { ptr, i64, i64, i64 } %.t309, 0
  %.t311 = insertvalue { ptr, i64, i64, i64 } undef, ptr %.t310, 0
  %.t312 = load { ptr, i64, i64, i64 }, ptr %lexer.addr
  %.t313 = extractvalue { ptr, i64, i64, i64 } %.t312, 1
  %.t314 = add i64 %.t313, 1
  %.t315 = insertvalue { ptr, i64, i64, i64 } %.t311, i64 %.t314, 1
  %.t316 = load { ptr, i64, i64, i64 }, ptr %lexer.addr
  %.t317 = extractvalue { ptr, i64, i64, i64 } %.t316, 2
  %.t318 = insertvalue { ptr, i64, i64, i64 } %.t315, i64 %.t317, 2
  %.t319 = load { ptr, i64, i64, i64 }, ptr %lexer.addr
  %.t320 = extractvalue { ptr, i64, i64, i64 } %.t319, 3
  %.t321 = add i64 %.t320, 1
  %.t322 = insertvalue { ptr, i64, i64, i64 } %.t318, i64 %.t321, 3
  ret { ptr, i64, i64, i64 } %.t322
  br label %if_end294
if_end294:
}

define { ptr, i64, i64, i64 } @skip_whitespace({ ptr, i64, i64, i64 } %lexer) {
entry:
  %lexer.addr = alloca { ptr, i64, i64, i64 }
  store { ptr, i64, i64, i64 } %lexer, ptr %lexer.addr
  %.t323 = load { ptr, i64, i64, i64 }, ptr %lexer.addr
  %.t324 = call i8 @peek({ ptr, i64, i64, i64 } %.t323)
  %c = alloca i8
  store i8 %.t324, ptr %c
  %.t328 = load i8, ptr %c
  %.t329 = icmp eq i8 %.t328, 255
  br i1 %.t329, label %if_then325, label %if_end327
if_then325:
  %.t330 = load { ptr, i64, i64, i64 }, ptr %lexer.addr
  ret { ptr, i64, i64, i64 } %.t330
  br label %if_end327
if_end327:
  %.t331 = load i8, ptr %c
  %.t332 = icmp eq i8 %.t331, 32
  %.t333 = load i8, ptr %c
  %.t334 = icmp eq i8 %.t333, 9
  %.t335 = or i1 %.t332, %.t334
  %.t336 = load i8, ptr %c
  %.t337 = icmp eq i8 %.t336, 10
  %.t338 = or i1 %.t335, %.t337
  %.t339 = load i8, ptr %c
  %.t340 = icmp eq i8 %.t339, 13
  %.t341 = or i1 %.t338, %.t340
  %is_whitespace = alloca i1
  store i1 %.t341, ptr %is_whitespace
  %.t345 = load i1, ptr %is_whitespace
  br i1 %.t345, label %if_then342, label %if_end344
if_then342:
  %.t346 = load { ptr, i64, i64, i64 }, ptr %lexer.addr
  %.t347 = call { ptr, i64, i64, i64 } @advance({ ptr, i64, i64, i64 } %.t346)
  %.t348 = call { ptr, i64, i64, i64 } @skip_whitespace({ ptr, i64, i64, i64 } %.t347)
  ret { ptr, i64, i64, i64 } %.t348
  br label %if_end344
if_end344:
  %.t352 = load i8, ptr %c
  %.t353 = icmp eq i8 %.t352, 47
  br i1 %.t353, label %if_then349, label %if_end351
if_then349:
  %.t354 = load { ptr, i64, i64, i64 }, ptr %lexer.addr
  %.t355 = call i8 @peek_ahead({ ptr, i64, i64, i64 } %.t354, i32 1)
  %next = alloca i8
  store i8 %.t355, ptr %next
  %.t359 = load i8, ptr %next
  %.t360 = icmp eq i8 %.t359, 47
  br i1 %.t360, label %if_then356, label %if_else357
if_then356:
  %.t361 = load { ptr, i64, i64, i64 }, ptr %lexer.addr
  %.t362 = call { ptr, i64, i64, i64 } @advance({ ptr, i64, i64, i64 } %.t361)
  %.t363 = call { ptr, i64, i64, i64 } @advance({ ptr, i64, i64, i64 } %.t362)
  %l = alloca { ptr, i64, i64, i64 }
  store { ptr, i64, i64, i64 } %.t363, ptr %l
  %.t364 = load { ptr, i64, i64, i64 }, ptr %l
  %.t365 = call i8 @peek({ ptr, i64, i64, i64 } %.t364)
  %ch = alloca i8
  store i8 %.t365, ptr %ch
  br label %while_header366
while_header366:
  %.t369 = load i8, ptr %ch
  %.t370 = icmp ne i8 %.t369, 255
  %.t371 = load i8, ptr %ch
  %.t372 = icmp ne i8 %.t371, 10
  %.t373 = and i1 %.t370, %.t372
  br i1 %.t373, label %while_body367, label %while_exit368
while_body367:
  %.t374 = load { ptr, i64, i64, i64 }, ptr %l
  %.t375 = call { ptr, i64, i64, i64 } @advance({ ptr, i64, i64, i64 } %.t374)
  store { ptr, i64, i64, i64 } %.t375, ptr %l
  %.t376 = load { ptr, i64, i64, i64 }, ptr %l
  %.t377 = call i8 @peek({ ptr, i64, i64, i64 } %.t376)
  store i8 %.t377, ptr %ch
  br label %while_header366
while_exit368:
  %.t378 = load { ptr, i64, i64, i64 }, ptr %l
  %.t379 = call { ptr, i64, i64, i64 } @skip_whitespace({ ptr, i64, i64, i64 } %.t378)
  ret { ptr, i64, i64, i64 } %.t379
  br label %if_end358
if_else357:
  %.t380 = load { ptr, i64, i64, i64 }, ptr %lexer.addr
  ret { ptr, i64, i64, i64 } %.t380
  br label %if_end358
if_end358:
  br label %if_end351
if_end351:
  %.t381 = load { ptr, i64, i64, i64 }, ptr %lexer.addr
  ret { ptr, i64, i64, i64 } %.t381
}

define { { { i64, [8 x i8] }, ptr, i64, i64 }, { ptr, i64, i64, i64 } } @next_token({ ptr, i64, i64, i64 } %lexer) {
entry:
  %lexer.addr = alloca { ptr, i64, i64, i64 }
  store { ptr, i64, i64, i64 } %lexer, ptr %lexer.addr
  %.t382 = load { ptr, i64, i64, i64 }, ptr %lexer.addr
  %.t383 = call { ptr, i64, i64, i64 } @skip_whitespace({ ptr, i64, i64, i64 } %.t382)
  %l = alloca { ptr, i64, i64, i64 }
  store { ptr, i64, i64, i64 } %.t383, ptr %l
  %.t384 = load { ptr, i64, i64, i64 }, ptr %l
  %.t385 = extractvalue { ptr, i64, i64, i64 } %.t384, 2
  %line = alloca i64
  store i64 %.t385, ptr %line
  %.t386 = load { ptr, i64, i64, i64 }, ptr %l
  %.t387 = extractvalue { ptr, i64, i64, i64 } %.t386, 3
  %column = alloca i64
  store i64 %.t387, ptr %column
  %.t388 = load { ptr, i64, i64, i64 }, ptr %l
  %.t389 = call i8 @peek({ ptr, i64, i64, i64 } %.t388)
  %c = alloca i8
  store i8 %.t389, ptr %c
  %.t393 = load i8, ptr %c
  %.t394 = icmp eq i8 %.t393, 255
  br i1 %.t394, label %if_then390, label %if_end392
if_then390:
  %.t395 = insertvalue { i64, [8 x i8] } undef, i64 52, 0
  %.t396 = insertvalue { { i64, [8 x i8] }, ptr, i64, i64 } undef, { i64, [8 x i8] } %.t395, 0
  %.t397 = insertvalue { { i64, [8 x i8] }, ptr, i64, i64 } %.t396, ptr getelementptr inbounds ([1 x i8], ptr @.str.0, i32 0, i32 0), 1
  %.t398 = load i64, ptr %line
  %.t399 = insertvalue { { i64, [8 x i8] }, ptr, i64, i64 } %.t397, i64 %.t398, 2
  %.t400 = load i64, ptr %column
  %.t401 = insertvalue { { i64, [8 x i8] }, ptr, i64, i64 } %.t399, i64 %.t400, 3
  %.t402 = insertvalue { { { i64, [8 x i8] }, ptr, i64, i64 }, { ptr, i64, i64, i64 } } undef, { { i64, [8 x i8] }, ptr, i64, i64 } %.t401, 0
  %.t403 = load { ptr, i64, i64, i64 }, ptr %l
  %.t404 = insertvalue { { { i64, [8 x i8] }, ptr, i64, i64 }, { ptr, i64, i64, i64 } } %.t402, { ptr, i64, i64, i64 } %.t403, 1
  ret { { { i64, [8 x i8] }, ptr, i64, i64 }, { ptr, i64, i64, i64 } } %.t404
  br label %if_end392
if_end392:
  %.t408 = load i8, ptr %c
  %.t409 = icmp eq i8 %.t408, 40
  br i1 %.t409, label %if_then405, label %if_else406
if_then405:
  %.t410 = insertvalue { i64, [8 x i8] } undef, i64 45, 0
  %.t411 = insertvalue { { i64, [8 x i8] }, ptr, i64, i64 } undef, { i64, [8 x i8] } %.t410, 0
  %.t412 = insertvalue { { i64, [8 x i8] }, ptr, i64, i64 } %.t411, ptr getelementptr inbounds ([2 x i8], ptr @.str.1, i32 0, i32 0), 1
  %.t413 = load i64, ptr %line
  %.t414 = insertvalue { { i64, [8 x i8] }, ptr, i64, i64 } %.t412, i64 %.t413, 2
  %.t415 = load i64, ptr %column
  %.t416 = insertvalue { { i64, [8 x i8] }, ptr, i64, i64 } %.t414, i64 %.t415, 3
  %.t417 = insertvalue { { { i64, [8 x i8] }, ptr, i64, i64 }, { ptr, i64, i64, i64 } } undef, { { i64, [8 x i8] }, ptr, i64, i64 } %.t416, 0
  %.t418 = load { ptr, i64, i64, i64 }, ptr %l
  %.t419 = call { ptr, i64, i64, i64 } @advance({ ptr, i64, i64, i64 } %.t418)
  %.t420 = insertvalue { { { i64, [8 x i8] }, ptr, i64, i64 }, { ptr, i64, i64, i64 } } %.t417, { ptr, i64, i64, i64 } %.t419, 1
  ret { { { i64, [8 x i8] }, ptr, i64, i64 }, { ptr, i64, i64, i64 } } %.t420
  br label %if_end407
if_else406:
  %.t424 = load i8, ptr %c
  %.t425 = icmp eq i8 %.t424, 41
  br i1 %.t425, label %if_then421, label %if_else422
if_then421:
  %.t426 = insertvalue { i64, [8 x i8] } undef, i64 46, 0
  %.t427 = insertvalue { { i64, [8 x i8] }, ptr, i64, i64 } undef, { i64, [8 x i8] } %.t426, 0
  %.t428 = insertvalue { { i64, [8 x i8] }, ptr, i64, i64 } %.t427, ptr getelementptr inbounds ([2 x i8], ptr @.str.2, i32 0, i32 0), 1
  %.t429 = load i64, ptr %line
  %.t430 = insertvalue { { i64, [8 x i8] }, ptr, i64, i64 } %.t428, i64 %.t429, 2
  %.t431 = load i64, ptr %column
  %.t432 = insertvalue { { i64, [8 x i8] }, ptr, i64, i64 } %.t430, i64 %.t431, 3
  %.t433 = insertvalue { { { i64, [8 x i8] }, ptr, i64, i64 }, { ptr, i64, i64, i64 } } undef, { { i64, [8 x i8] }, ptr, i64, i64 } %.t432, 0
  %.t434 = load { ptr, i64, i64, i64 }, ptr %l
  %.t435 = call { ptr, i64, i64, i64 } @advance({ ptr, i64, i64, i64 } %.t434)
  %.t436 = insertvalue { { { i64, [8 x i8] }, ptr, i64, i64 }, { ptr, i64, i64, i64 } } %.t433, { ptr, i64, i64, i64 } %.t435, 1
  ret { { { i64, [8 x i8] }, ptr, i64, i64 }, { ptr, i64, i64, i64 } } %.t436
  br label %if_end423
if_else422:
  %.t440 = load i8, ptr %c
  %.t441 = icmp eq i8 %.t440, 91
  br i1 %.t441, label %if_then437, label %if_else438
if_then437:
  %.t442 = insertvalue { i64, [8 x i8] } undef, i64 47, 0
  %.t443 = insertvalue { { i64, [8 x i8] }, ptr, i64, i64 } undef, { i64, [8 x i8] } %.t442, 0
  %.t444 = insertvalue { { i64, [8 x i8] }, ptr, i64, i64 } %.t443, ptr getelementptr inbounds ([2 x i8], ptr @.str.3, i32 0, i32 0), 1
  %.t445 = load i64, ptr %line
  %.t446 = insertvalue { { i64, [8 x i8] }, ptr, i64, i64 } %.t444, i64 %.t445, 2
  %.t447 = load i64, ptr %column
  %.t448 = insertvalue { { i64, [8 x i8] }, ptr, i64, i64 } %.t446, i64 %.t447, 3
  %.t449 = insertvalue { { { i64, [8 x i8] }, ptr, i64, i64 }, { ptr, i64, i64, i64 } } undef, { { i64, [8 x i8] }, ptr, i64, i64 } %.t448, 0
  %.t450 = load { ptr, i64, i64, i64 }, ptr %l
  %.t451 = call { ptr, i64, i64, i64 } @advance({ ptr, i64, i64, i64 } %.t450)
  %.t452 = insertvalue { { { i64, [8 x i8] }, ptr, i64, i64 }, { ptr, i64, i64, i64 } } %.t449, { ptr, i64, i64, i64 } %.t451, 1
  ret { { { i64, [8 x i8] }, ptr, i64, i64 }, { ptr, i64, i64, i64 } } %.t452
  br label %if_end439
if_else438:
  %.t456 = load i8, ptr %c
  %.t457 = icmp eq i8 %.t456, 93
  br i1 %.t457, label %if_then453, label %if_else454
if_then453:
  %.t458 = insertvalue { i64, [8 x i8] } undef, i64 48, 0
  %.t459 = insertvalue { { i64, [8 x i8] }, ptr, i64, i64 } undef, { i64, [8 x i8] } %.t458, 0
  %.t460 = insertvalue { { i64, [8 x i8] }, ptr, i64, i64 } %.t459, ptr getelementptr inbounds ([2 x i8], ptr @.str.4, i32 0, i32 0), 1
  %.t461 = load i64, ptr %line
  %.t462 = insertvalue { { i64, [8 x i8] }, ptr, i64, i64 } %.t460, i64 %.t461, 2
  %.t463 = load i64, ptr %column
  %.t464 = insertvalue { { i64, [8 x i8] }, ptr, i64, i64 } %.t462, i64 %.t463, 3
  %.t465 = insertvalue { { { i64, [8 x i8] }, ptr, i64, i64 }, { ptr, i64, i64, i64 } } undef, { { i64, [8 x i8] }, ptr, i64, i64 } %.t464, 0
  %.t466 = load { ptr, i64, i64, i64 }, ptr %l
  %.t467 = call { ptr, i64, i64, i64 } @advance({ ptr, i64, i64, i64 } %.t466)
  %.t468 = insertvalue { { { i64, [8 x i8] }, ptr, i64, i64 }, { ptr, i64, i64, i64 } } %.t465, { ptr, i64, i64, i64 } %.t467, 1
  ret { { { i64, [8 x i8] }, ptr, i64, i64 }, { ptr, i64, i64, i64 } } %.t468
  br label %if_end455
if_else454:
  %.t472 = load i8, ptr %c
  %.t473 = icmp eq i8 %.t472, 123
  br i1 %.t473, label %if_then469, label %if_else470
if_then469:
  %.t474 = insertvalue { i64, [8 x i8] } undef, i64 49, 0
  %.t475 = insertvalue { { i64, [8 x i8] }, ptr, i64, i64 } undef, { i64, [8 x i8] } %.t474, 0
  %.t476 = insertvalue { { i64, [8 x i8] }, ptr, i64, i64 } %.t475, ptr getelementptr inbounds ([2 x i8], ptr @.str.5, i32 0, i32 0), 1
  %.t477 = load i64, ptr %line
  %.t478 = insertvalue { { i64, [8 x i8] }, ptr, i64, i64 } %.t476, i64 %.t477, 2
  %.t479 = load i64, ptr %column
  %.t480 = insertvalue { { i64, [8 x i8] }, ptr, i64, i64 } %.t478, i64 %.t479, 3
  %.t481 = insertvalue { { { i64, [8 x i8] }, ptr, i64, i64 }, { ptr, i64, i64, i64 } } undef, { { i64, [8 x i8] }, ptr, i64, i64 } %.t480, 0
  %.t482 = load { ptr, i64, i64, i64 }, ptr %l
  %.t483 = call { ptr, i64, i64, i64 } @advance({ ptr, i64, i64, i64 } %.t482)
  %.t484 = insertvalue { { { i64, [8 x i8] }, ptr, i64, i64 }, { ptr, i64, i64, i64 } } %.t481, { ptr, i64, i64, i64 } %.t483, 1
  ret { { { i64, [8 x i8] }, ptr, i64, i64 }, { ptr, i64, i64, i64 } } %.t484
  br label %if_end471
if_else470:
  %.t488 = load i8, ptr %c
  %.t489 = icmp eq i8 %.t488, 125
  br i1 %.t489, label %if_then485, label %if_else486
if_then485:
  %.t490 = insertvalue { i64, [8 x i8] } undef, i64 50, 0
  %.t491 = insertvalue { { i64, [8 x i8] }, ptr, i64, i64 } undef, { i64, [8 x i8] } %.t490, 0
  %.t492 = insertvalue { { i64, [8 x i8] }, ptr, i64, i64 } %.t491, ptr getelementptr inbounds ([2 x i8], ptr @.str.6, i32 0, i32 0), 1
  %.t493 = load i64, ptr %line
  %.t494 = insertvalue { { i64, [8 x i8] }, ptr, i64, i64 } %.t492, i64 %.t493, 2
  %.t495 = load i64, ptr %column
  %.t496 = insertvalue { { i64, [8 x i8] }, ptr, i64, i64 } %.t494, i64 %.t495, 3
  %.t497 = insertvalue { { { i64, [8 x i8] }, ptr, i64, i64 }, { ptr, i64, i64, i64 } } undef, { { i64, [8 x i8] }, ptr, i64, i64 } %.t496, 0
  %.t498 = load { ptr, i64, i64, i64 }, ptr %l
  %.t499 = call { ptr, i64, i64, i64 } @advance({ ptr, i64, i64, i64 } %.t498)
  %.t500 = insertvalue { { { i64, [8 x i8] }, ptr, i64, i64 }, { ptr, i64, i64, i64 } } %.t497, { ptr, i64, i64, i64 } %.t499, 1
  ret { { { i64, [8 x i8] }, ptr, i64, i64 }, { ptr, i64, i64, i64 } } %.t500
  br label %if_end487
if_else486:
  %.t504 = load i8, ptr %c
  %.t505 = icmp eq i8 %.t504, 59
  br i1 %.t505, label %if_then501, label %if_else502
if_then501:
  %.t506 = insertvalue { i64, [8 x i8] } undef, i64 39, 0
  %.t507 = insertvalue { { i64, [8 x i8] }, ptr, i64, i64 } undef, { i64, [8 x i8] } %.t506, 0
  %.t508 = insertvalue { { i64, [8 x i8] }, ptr, i64, i64 } %.t507, ptr getelementptr inbounds ([2 x i8], ptr @.str.7, i32 0, i32 0), 1
  %.t509 = load i64, ptr %line
  %.t510 = insertvalue { { i64, [8 x i8] }, ptr, i64, i64 } %.t508, i64 %.t509, 2
  %.t511 = load i64, ptr %column
  %.t512 = insertvalue { { i64, [8 x i8] }, ptr, i64, i64 } %.t510, i64 %.t511, 3
  %.t513 = insertvalue { { { i64, [8 x i8] }, ptr, i64, i64 }, { ptr, i64, i64, i64 } } undef, { { i64, [8 x i8] }, ptr, i64, i64 } %.t512, 0
  %.t514 = load { ptr, i64, i64, i64 }, ptr %l
  %.t515 = call { ptr, i64, i64, i64 } @advance({ ptr, i64, i64, i64 } %.t514)
  %.t516 = insertvalue { { { i64, [8 x i8] }, ptr, i64, i64 }, { ptr, i64, i64, i64 } } %.t513, { ptr, i64, i64, i64 } %.t515, 1
  ret { { { i64, [8 x i8] }, ptr, i64, i64 }, { ptr, i64, i64, i64 } } %.t516
  br label %if_end503
if_else502:
  %.t520 = load i8, ptr %c
  %.t521 = icmp eq i8 %.t520, 44
  br i1 %.t521, label %if_then517, label %if_else518
if_then517:
  %.t522 = insertvalue { i64, [8 x i8] } undef, i64 40, 0
  %.t523 = insertvalue { { i64, [8 x i8] }, ptr, i64, i64 } undef, { i64, [8 x i8] } %.t522, 0
  %.t524 = insertvalue { { i64, [8 x i8] }, ptr, i64, i64 } %.t523, ptr getelementptr inbounds ([2 x i8], ptr @.str.8, i32 0, i32 0), 1
  %.t525 = load i64, ptr %line
  %.t526 = insertvalue { { i64, [8 x i8] }, ptr, i64, i64 } %.t524, i64 %.t525, 2
  %.t527 = load i64, ptr %column
  %.t528 = insertvalue { { i64, [8 x i8] }, ptr, i64, i64 } %.t526, i64 %.t527, 3
  %.t529 = insertvalue { { { i64, [8 x i8] }, ptr, i64, i64 }, { ptr, i64, i64, i64 } } undef, { { i64, [8 x i8] }, ptr, i64, i64 } %.t528, 0
  %.t530 = load { ptr, i64, i64, i64 }, ptr %l
  %.t531 = call { ptr, i64, i64, i64 } @advance({ ptr, i64, i64, i64 } %.t530)
  %.t532 = insertvalue { { { i64, [8 x i8] }, ptr, i64, i64 }, { ptr, i64, i64, i64 } } %.t529, { ptr, i64, i64, i64 } %.t531, 1
  ret { { { i64, [8 x i8] }, ptr, i64, i64 }, { ptr, i64, i64, i64 } } %.t532
  br label %if_end519
if_else518:
  %.t536 = load i8, ptr %c
  %.t537 = icmp eq i8 %.t536, 46
  br i1 %.t537, label %if_then533, label %if_else534
if_then533:
  %.t538 = insertvalue { i64, [8 x i8] } undef, i64 41, 0
  %.t539 = insertvalue { { i64, [8 x i8] }, ptr, i64, i64 } undef, { i64, [8 x i8] } %.t538, 0
  %.t540 = insertvalue { { i64, [8 x i8] }, ptr, i64, i64 } %.t539, ptr getelementptr inbounds ([2 x i8], ptr @.str.9, i32 0, i32 0), 1
  %.t541 = load i64, ptr %line
  %.t542 = insertvalue { { i64, [8 x i8] }, ptr, i64, i64 } %.t540, i64 %.t541, 2
  %.t543 = load i64, ptr %column
  %.t544 = insertvalue { { i64, [8 x i8] }, ptr, i64, i64 } %.t542, i64 %.t543, 3
  %.t545 = insertvalue { { { i64, [8 x i8] }, ptr, i64, i64 }, { ptr, i64, i64, i64 } } undef, { { i64, [8 x i8] }, ptr, i64, i64 } %.t544, 0
  %.t546 = load { ptr, i64, i64, i64 }, ptr %l
  %.t547 = call { ptr, i64, i64, i64 } @advance({ ptr, i64, i64, i64 } %.t546)
  %.t548 = insertvalue { { { i64, [8 x i8] }, ptr, i64, i64 }, { ptr, i64, i64, i64 } } %.t545, { ptr, i64, i64, i64 } %.t547, 1
  ret { { { i64, [8 x i8] }, ptr, i64, i64 }, { ptr, i64, i64, i64 } } %.t548
  br label %if_end535
if_else534:
  %.t552 = load i8, ptr %c
  %.t553 = icmp eq i8 %.t552, 58
  br i1 %.t553, label %if_then549, label %if_else550
if_then549:
  %.t554 = insertvalue { i64, [8 x i8] } undef, i64 38, 0
  %.t555 = insertvalue { { i64, [8 x i8] }, ptr, i64, i64 } undef, { i64, [8 x i8] } %.t554, 0
  %.t556 = insertvalue { { i64, [8 x i8] }, ptr, i64, i64 } %.t555, ptr getelementptr inbounds ([2 x i8], ptr @.str.10, i32 0, i32 0), 1
  %.t557 = load i64, ptr %line
  %.t558 = insertvalue { { i64, [8 x i8] }, ptr, i64, i64 } %.t556, i64 %.t557, 2
  %.t559 = load i64, ptr %column
  %.t560 = insertvalue { { i64, [8 x i8] }, ptr, i64, i64 } %.t558, i64 %.t559, 3
  %.t561 = insertvalue { { { i64, [8 x i8] }, ptr, i64, i64 }, { ptr, i64, i64, i64 } } undef, { { i64, [8 x i8] }, ptr, i64, i64 } %.t560, 0
  %.t562 = load { ptr, i64, i64, i64 }, ptr %l
  %.t563 = call { ptr, i64, i64, i64 } @advance({ ptr, i64, i64, i64 } %.t562)
  %.t564 = insertvalue { { { i64, [8 x i8] }, ptr, i64, i64 }, { ptr, i64, i64, i64 } } %.t561, { ptr, i64, i64, i64 } %.t563, 1
  ret { { { i64, [8 x i8] }, ptr, i64, i64 }, { ptr, i64, i64, i64 } } %.t564
  br label %if_end551
if_else550:
  %.t565 = insertvalue { i64, [8 x i8] } undef, i64 52, 0
  %.t566 = insertvalue { { i64, [8 x i8] }, ptr, i64, i64 } undef, { i64, [8 x i8] } %.t565, 0
  %.t567 = insertvalue { { i64, [8 x i8] }, ptr, i64, i64 } %.t566, ptr getelementptr inbounds ([1 x i8], ptr @.str.11, i32 0, i32 0), 1
  %.t568 = load i64, ptr %line
  %.t569 = insertvalue { { i64, [8 x i8] }, ptr, i64, i64 } %.t567, i64 %.t568, 2
  %.t570 = load i64, ptr %column
  %.t571 = insertvalue { { i64, [8 x i8] }, ptr, i64, i64 } %.t569, i64 %.t570, 3
  %.t572 = insertvalue { { { i64, [8 x i8] }, ptr, i64, i64 }, { ptr, i64, i64, i64 } } undef, { { i64, [8 x i8] }, ptr, i64, i64 } %.t571, 0
  %.t573 = load { ptr, i64, i64, i64 }, ptr %l
  %.t574 = call { ptr, i64, i64, i64 } @advance({ ptr, i64, i64, i64 } %.t573)
  %.t575 = insertvalue { { { i64, [8 x i8] }, ptr, i64, i64 }, { ptr, i64, i64, i64 } } %.t572, { ptr, i64, i64, i64 } %.t574, 1
  ret { { { i64, [8 x i8] }, ptr, i64, i64 }, { ptr, i64, i64, i64 } } %.t575
  br label %if_end551
if_end551:
  br label %if_end535
if_end535:
  br label %if_end519
if_end519:
  br label %if_end503
if_end503:
  br label %if_end487
if_end487:
  br label %if_end471
if_end471:
  br label %if_end455
if_end455:
  br label %if_end439
if_end439:
  br label %if_end423
if_end423:
  br label %if_end407
if_end407:
}


@.str.0 = private unnamed_addr constant [1 x i8] c"\00"
@.str.1 = private unnamed_addr constant [2 x i8] c"(\00"
@.str.2 = private unnamed_addr constant [2 x i8] c")\00"
@.str.3 = private unnamed_addr constant [2 x i8] c"[\00"
@.str.4 = private unnamed_addr constant [2 x i8] c"]\00"
@.str.5 = private unnamed_addr constant [2 x i8] c"{\00"
@.str.6 = private unnamed_addr constant [2 x i8] c"}\00"
@.str.7 = private unnamed_addr constant [2 x i8] c";\00"
@.str.8 = private unnamed_addr constant [2 x i8] c",\00"
@.str.9 = private unnamed_addr constant [2 x i8] c".\00"
@.str.10 = private unnamed_addr constant [2 x i8] c":\00"
@.str.11 = private unnamed_addr constant [1 x i8] c"\00"
define ptr @ptr__read(ptr %self, i64 %typ) {
entry:
  %self.addr = alloca ptr
  store ptr %self, ptr %self.addr
  %typ.addr = alloca i64
  store i64 %typ, ptr %typ.addr
  %.t576 = load ptr, ptr %self.addr
  %.t577 = load i64, ptr %typ.addr
  %.t578 = alloca i64
  %.t579 = icmp eq i64 %.t577, 1
  br i1 %.t579, label %bool_case580, label %i8_check581

bool_case580:
  %.t583 = load i1, ptr %.t576
  %.t584 = zext i1 %.t583 to i64
  store i64 %.t584, ptr %.t578
  br label %continue582

i8_check581:
  %.t585 = icmp eq i64 %.t577, 2
  br i1 %.t585, label %i8_block586, label %default587

i8_block586:
  %.t588 = load i8, ptr %.t576
  %.t589 = sext i8 %.t588 to i64
  store i64 %.t589, ptr %.t578
  br label %continue582

default587:
  %.t590 = load i64, ptr %.t576
  store i64 %.t590, ptr %.t578
  br label %continue582

continue582:
  %.t591 = load i64, ptr %.t578
  %.t592 = inttoptr i64 %.t591 to ptr
  ret ptr %.t592
}

define {  } @ptr__write(ptr %self, ptr %value) {
entry:
  %self.addr = alloca ptr
  store ptr %self, ptr %self.addr
  %value.addr = alloca ptr
  store ptr %value, ptr %value.addr
  %.t593 = load ptr, ptr %self.addr
  %.t594 = load ptr, ptr %value.addr
  %.t595 = ptrtoint ptr %.t594 to i64
  store i64 %.t595, ptr %.t593
  ret {  } undef
}

define ptr @ptr__offset(ptr %self, i64 %off) {
entry:
  %self.addr = alloca ptr
  store ptr %self, ptr %self.addr
  %off.addr = alloca i64
  store i64 %off, ptr %off.addr
  %.t596 = load ptr, ptr %self.addr
  %.t597 = load i64, ptr %off.addr
  %.t598 = getelementptr i64, ptr %.t596, i64 %.t597
  ret ptr %.t598
}

define i1 @Result$$i64$Error$$__is_ok({ i64, [8 x i8] } %res) {
entry:
  %res.addr = alloca { i64, [8 x i8] }
  store { i64, [8 x i8] } %res, ptr %res.addr
  %.t599 = load { i64, [8 x i8] }, ptr %res.addr
  %.t600 = extractvalue { i64, [8 x i8] } %.t599, 0
  switch i64 %.t600, label %match_arm602 [
    i64 0, label %match_arm602, 
    i64 1, label %match_arm603
  ]
match_arm602:
  %.t604 = alloca { i64, [8 x i8] }
  store { i64, [8 x i8] } %.t599, ptr %.t604
  %.t605 = getelementptr inbounds { i64, [8 x i8] }, ptr %.t604, i32 0, i32 1
  %.t606 = getelementptr inbounds i8, ptr %.t605, i32 0
  %.t607 = alloca i64
  %.t608 = load i64, ptr %.t606
  store i64 %.t608, ptr %.t607
  br label %match_merge601
match_arm603:
  %.t609 = alloca { i64, [8 x i8] }
  store { i64, [8 x i8] } %.t599, ptr %.t609
  %.t610 = getelementptr inbounds { i64, [8 x i8] }, ptr %.t609, i32 0, i32 1
  %.t611 = getelementptr inbounds i8, ptr %.t610, i32 0
  %.t612 = alloca { i64, [8 x i8] }
  %.t613 = load { i64, [8 x i8] }, ptr %.t611
  store { i64, [8 x i8] } %.t613, ptr %.t612
  br label %match_merge601
match_merge601:
  %.t614 = phi i1 [ 1, %match_arm602 ], [ 0, %match_arm603 ]
  ret i1 %.t614
}

define i1 @Result$$i64$Error$$__is_err({ i64, [8 x i8] } %res) {
entry:
  %res.addr = alloca { i64, [8 x i8] }
  store { i64, [8 x i8] } %res, ptr %res.addr
  %.t615 = load { i64, [8 x i8] }, ptr %res.addr
  %.t616 = extractvalue { i64, [8 x i8] } %.t615, 0
  switch i64 %.t616, label %match_arm618 [
    i64 0, label %match_arm618, 
    i64 1, label %match_arm619
  ]
match_arm618:
  %.t620 = alloca { i64, [8 x i8] }
  store { i64, [8 x i8] } %.t615, ptr %.t620
  %.t621 = getelementptr inbounds { i64, [8 x i8] }, ptr %.t620, i32 0, i32 1
  %.t622 = getelementptr inbounds i8, ptr %.t621, i32 0
  %.t623 = alloca i64
  %.t624 = load i64, ptr %.t622
  store i64 %.t624, ptr %.t623
  br label %match_merge617
match_arm619:
  %.t625 = alloca { i64, [8 x i8] }
  store { i64, [8 x i8] } %.t615, ptr %.t625
  %.t626 = getelementptr inbounds { i64, [8 x i8] }, ptr %.t625, i32 0, i32 1
  %.t627 = getelementptr inbounds i8, ptr %.t626, i32 0
  %.t628 = alloca { i64, [8 x i8] }
  %.t629 = load { i64, [8 x i8] }, ptr %.t627
  store { i64, [8 x i8] } %.t629, ptr %.t628
  br label %match_merge617
match_merge617:
  %.t630 = phi i1 [ 0, %match_arm618 ], [ 1, %match_arm619 ]
  ret i1 %.t630
}

define i64 @Result$$i64$Error$$__unwrap_or({ i64, [8 x i8] } %res, i64 %fallback) {
entry:
  %res.addr = alloca { i64, [8 x i8] }
  store { i64, [8 x i8] } %res, ptr %res.addr
  %fallback.addr = alloca i64
  store i64 %fallback, ptr %fallback.addr
  %.t631 = load { i64, [8 x i8] }, ptr %res.addr
  %.t632 = extractvalue { i64, [8 x i8] } %.t631, 0
  switch i64 %.t632, label %match_arm634 [
    i64 0, label %match_arm634, 
    i64 1, label %match_arm635
  ]
match_arm634:
  %.t636 = alloca { i64, [8 x i8] }
  store { i64, [8 x i8] } %.t631, ptr %.t636
  %.t637 = getelementptr inbounds { i64, [8 x i8] }, ptr %.t636, i32 0, i32 1
  %.t638 = getelementptr inbounds i8, ptr %.t637, i32 0
  %.t639 = alloca i64
  %.t640 = load i64, ptr %.t638
  store i64 %.t640, ptr %.t639
  %.t641 = load i64, ptr %.t639
  %.t642 = trunc i64 %.t641 to i32
  br label %match_merge633
match_arm635:
  %.t643 = alloca { i64, [8 x i8] }
  store { i64, [8 x i8] } %.t631, ptr %.t643
  %.t644 = getelementptr inbounds { i64, [8 x i8] }, ptr %.t643, i32 0, i32 1
  %.t645 = getelementptr inbounds i8, ptr %.t644, i32 0
  %.t646 = alloca { i64, [8 x i8] }
  %.t647 = load { i64, [8 x i8] }, ptr %.t645
  store { i64, [8 x i8] } %.t647, ptr %.t646
  %.t648 = load i64, ptr %fallback.addr
  %.t649 = trunc i64 %.t648 to i32
  br label %match_merge633
match_merge633:
  %.t650 = phi i32 [ %.t642, %match_arm634 ], [ %.t649, %match_arm635 ]
  %.t651 = sext i32 %.t650 to i64
  ret i64 %.t651
}

define i1 @Result$$u64$Error$$__is_ok({ i64, [8 x i8] } %res) {
entry:
  %res.addr = alloca { i64, [8 x i8] }
  store { i64, [8 x i8] } %res, ptr %res.addr
  %.t652 = load { i64, [8 x i8] }, ptr %res.addr
  %.t653 = extractvalue { i64, [8 x i8] } %.t652, 0
  switch i64 %.t653, label %match_arm655 [
    i64 0, label %match_arm655, 
    i64 1, label %match_arm656
  ]
match_arm655:
  %.t657 = alloca { i64, [8 x i8] }
  store { i64, [8 x i8] } %.t652, ptr %.t657
  %.t658 = getelementptr inbounds { i64, [8 x i8] }, ptr %.t657, i32 0, i32 1
  %.t659 = getelementptr inbounds i8, ptr %.t658, i32 0
  %.t660 = alloca i64
  %.t661 = load i64, ptr %.t659
  store i64 %.t661, ptr %.t660
  br label %match_merge654
match_arm656:
  %.t662 = alloca { i64, [8 x i8] }
  store { i64, [8 x i8] } %.t652, ptr %.t662
  %.t663 = getelementptr inbounds { i64, [8 x i8] }, ptr %.t662, i32 0, i32 1
  %.t664 = getelementptr inbounds i8, ptr %.t663, i32 0
  %.t665 = alloca { i64, [8 x i8] }
  %.t666 = load { i64, [8 x i8] }, ptr %.t664
  store { i64, [8 x i8] } %.t666, ptr %.t665
  br label %match_merge654
match_merge654:
  %.t667 = phi i1 [ 1, %match_arm655 ], [ 0, %match_arm656 ]
  ret i1 %.t667
}

define i1 @Result$$u64$Error$$__is_err({ i64, [8 x i8] } %res) {
entry:
  %res.addr = alloca { i64, [8 x i8] }
  store { i64, [8 x i8] } %res, ptr %res.addr
  %.t668 = load { i64, [8 x i8] }, ptr %res.addr
  %.t669 = extractvalue { i64, [8 x i8] } %.t668, 0
  switch i64 %.t669, label %match_arm671 [
    i64 0, label %match_arm671, 
    i64 1, label %match_arm672
  ]
match_arm671:
  %.t673 = alloca { i64, [8 x i8] }
  store { i64, [8 x i8] } %.t668, ptr %.t673
  %.t674 = getelementptr inbounds { i64, [8 x i8] }, ptr %.t673, i32 0, i32 1
  %.t675 = getelementptr inbounds i8, ptr %.t674, i32 0
  %.t676 = alloca i64
  %.t677 = load i64, ptr %.t675
  store i64 %.t677, ptr %.t676
  br label %match_merge670
match_arm672:
  %.t678 = alloca { i64, [8 x i8] }
  store { i64, [8 x i8] } %.t668, ptr %.t678
  %.t679 = getelementptr inbounds { i64, [8 x i8] }, ptr %.t678, i32 0, i32 1
  %.t680 = getelementptr inbounds i8, ptr %.t679, i32 0
  %.t681 = alloca { i64, [8 x i8] }
  %.t682 = load { i64, [8 x i8] }, ptr %.t680
  store { i64, [8 x i8] } %.t682, ptr %.t681
  br label %match_merge670
match_merge670:
  %.t683 = phi i1 [ 0, %match_arm671 ], [ 1, %match_arm672 ]
  ret i1 %.t683
}

define i64 @Result$$u64$Error$$__unwrap_or({ i64, [8 x i8] } %res, i64 %fallback) {
entry:
  %res.addr = alloca { i64, [8 x i8] }
  store { i64, [8 x i8] } %res, ptr %res.addr
  %fallback.addr = alloca i64
  store i64 %fallback, ptr %fallback.addr
  %.t684 = load { i64, [8 x i8] }, ptr %res.addr
  %.t685 = extractvalue { i64, [8 x i8] } %.t684, 0
  switch i64 %.t685, label %match_arm687 [
    i64 0, label %match_arm687, 
    i64 1, label %match_arm688
  ]
match_arm687:
  %.t689 = alloca { i64, [8 x i8] }
  store { i64, [8 x i8] } %.t684, ptr %.t689
  %.t690 = getelementptr inbounds { i64, [8 x i8] }, ptr %.t689, i32 0, i32 1
  %.t691 = getelementptr inbounds i8, ptr %.t690, i32 0
  %.t692 = alloca i64
  %.t693 = load i64, ptr %.t691
  store i64 %.t693, ptr %.t692
  %.t694 = load i64, ptr %.t692
  %.t695 = trunc i64 %.t694 to i32
  br label %match_merge686
match_arm688:
  %.t696 = alloca { i64, [8 x i8] }
  store { i64, [8 x i8] } %.t684, ptr %.t696
  %.t697 = getelementptr inbounds { i64, [8 x i8] }, ptr %.t696, i32 0, i32 1
  %.t698 = getelementptr inbounds i8, ptr %.t697, i32 0
  %.t699 = alloca { i64, [8 x i8] }
  %.t700 = load { i64, [8 x i8] }, ptr %.t698
  store { i64, [8 x i8] } %.t700, ptr %.t699
  %.t701 = load i64, ptr %fallback.addr
  %.t702 = trunc i64 %.t701 to i32
  br label %match_merge686
match_merge686:
  %.t703 = phi i32 [ %.t695, %match_arm687 ], [ %.t702, %match_arm688 ]
  %.t704 = sext i32 %.t703 to i64
  ret i64 %.t704
}

define i1 @Result$$str$Error$$__is_ok({ i64, [8 x i8] } %res) {
entry:
  %res.addr = alloca { i64, [8 x i8] }
  store { i64, [8 x i8] } %res, ptr %res.addr
  %.t705 = load { i64, [8 x i8] }, ptr %res.addr
  %.t706 = extractvalue { i64, [8 x i8] } %.t705, 0
  switch i64 %.t706, label %match_arm708 [
    i64 0, label %match_arm708, 
    i64 1, label %match_arm709
  ]
match_arm708:
  %.t710 = alloca { i64, [8 x i8] }
  store { i64, [8 x i8] } %.t705, ptr %.t710
  %.t711 = getelementptr inbounds { i64, [8 x i8] }, ptr %.t710, i32 0, i32 1
  %.t712 = getelementptr inbounds i8, ptr %.t711, i32 0
  %.t713 = alloca ptr
  %.t714 = load ptr, ptr %.t712
  store ptr %.t714, ptr %.t713
  br label %match_merge707
match_arm709:
  %.t715 = alloca { i64, [8 x i8] }
  store { i64, [8 x i8] } %.t705, ptr %.t715
  %.t716 = getelementptr inbounds { i64, [8 x i8] }, ptr %.t715, i32 0, i32 1
  %.t717 = getelementptr inbounds i8, ptr %.t716, i32 0
  %.t718 = alloca { i64, [8 x i8] }
  %.t719 = load { i64, [8 x i8] }, ptr %.t717
  store { i64, [8 x i8] } %.t719, ptr %.t718
  br label %match_merge707
match_merge707:
  %.t720 = phi i1 [ 1, %match_arm708 ], [ 0, %match_arm709 ]
  ret i1 %.t720
}

define i1 @Result$$str$Error$$__is_err({ i64, [8 x i8] } %res) {
entry:
  %res.addr = alloca { i64, [8 x i8] }
  store { i64, [8 x i8] } %res, ptr %res.addr
  %.t721 = load { i64, [8 x i8] }, ptr %res.addr
  %.t722 = extractvalue { i64, [8 x i8] } %.t721, 0
  switch i64 %.t722, label %match_arm724 [
    i64 0, label %match_arm724, 
    i64 1, label %match_arm725
  ]
match_arm724:
  %.t726 = alloca { i64, [8 x i8] }
  store { i64, [8 x i8] } %.t721, ptr %.t726
  %.t727 = getelementptr inbounds { i64, [8 x i8] }, ptr %.t726, i32 0, i32 1
  %.t728 = getelementptr inbounds i8, ptr %.t727, i32 0
  %.t729 = alloca ptr
  %.t730 = load ptr, ptr %.t728
  store ptr %.t730, ptr %.t729
  br label %match_merge723
match_arm725:
  %.t731 = alloca { i64, [8 x i8] }
  store { i64, [8 x i8] } %.t721, ptr %.t731
  %.t732 = getelementptr inbounds { i64, [8 x i8] }, ptr %.t731, i32 0, i32 1
  %.t733 = getelementptr inbounds i8, ptr %.t732, i32 0
  %.t734 = alloca { i64, [8 x i8] }
  %.t735 = load { i64, [8 x i8] }, ptr %.t733
  store { i64, [8 x i8] } %.t735, ptr %.t734
  br label %match_merge723
match_merge723:
  %.t736 = phi i1 [ 0, %match_arm724 ], [ 1, %match_arm725 ]
  ret i1 %.t736
}

define ptr @Result$$str$Error$$__unwrap_or({ i64, [8 x i8] } %res, ptr %fallback) {
entry:
  %res.addr = alloca { i64, [8 x i8] }
  store { i64, [8 x i8] } %res, ptr %res.addr
  %fallback.addr = alloca ptr
  store ptr %fallback, ptr %fallback.addr
  %.t737 = load { i64, [8 x i8] }, ptr %res.addr
  %.t738 = extractvalue { i64, [8 x i8] } %.t737, 0
  switch i64 %.t738, label %match_arm740 [
    i64 0, label %match_arm740, 
    i64 1, label %match_arm741
  ]
match_arm740:
  %.t742 = alloca { i64, [8 x i8] }
  store { i64, [8 x i8] } %.t737, ptr %.t742
  %.t743 = getelementptr inbounds { i64, [8 x i8] }, ptr %.t742, i32 0, i32 1
  %.t744 = getelementptr inbounds i8, ptr %.t743, i32 0
  %.t745 = alloca ptr
  %.t746 = load ptr, ptr %.t744
  store ptr %.t746, ptr %.t745
  %.t747 = load ptr, ptr %.t745
  %.t748 = ptrtoint ptr %.t747 to i32
  br label %match_merge739
match_arm741:
  %.t749 = alloca { i64, [8 x i8] }
  store { i64, [8 x i8] } %.t737, ptr %.t749
  %.t750 = getelementptr inbounds { i64, [8 x i8] }, ptr %.t749, i32 0, i32 1
  %.t751 = getelementptr inbounds i8, ptr %.t750, i32 0
  %.t752 = alloca { i64, [8 x i8] }
  %.t753 = load { i64, [8 x i8] }, ptr %.t751
  store { i64, [8 x i8] } %.t753, ptr %.t752
  %.t754 = load ptr, ptr %fallback.addr
  %.t755 = ptrtoint ptr %.t754 to i32
  br label %match_merge739
match_merge739:
  %.t756 = phi i32 [ %.t748, %match_arm740 ], [ %.t755, %match_arm741 ]
  %.t757 = inttoptr i32 %.t756 to ptr
  ret ptr %.t757
}

define i1 @Result$$ptr$Error$$__is_ok({ i64, [8 x i8] } %res) {
entry:
  %res.addr = alloca { i64, [8 x i8] }
  store { i64, [8 x i8] } %res, ptr %res.addr
  %.t758 = load { i64, [8 x i8] }, ptr %res.addr
  %.t759 = extractvalue { i64, [8 x i8] } %.t758, 0
  switch i64 %.t759, label %match_arm761 [
    i64 0, label %match_arm761, 
    i64 1, label %match_arm762
  ]
match_arm761:
  %.t763 = alloca { i64, [8 x i8] }
  store { i64, [8 x i8] } %.t758, ptr %.t763
  %.t764 = getelementptr inbounds { i64, [8 x i8] }, ptr %.t763, i32 0, i32 1
  %.t765 = getelementptr inbounds i8, ptr %.t764, i32 0
  %.t766 = alloca ptr
  %.t767 = load ptr, ptr %.t765
  store ptr %.t767, ptr %.t766
  br label %match_merge760
match_arm762:
  %.t768 = alloca { i64, [8 x i8] }
  store { i64, [8 x i8] } %.t758, ptr %.t768
  %.t769 = getelementptr inbounds { i64, [8 x i8] }, ptr %.t768, i32 0, i32 1
  %.t770 = getelementptr inbounds i8, ptr %.t769, i32 0
  %.t771 = alloca { i64, [8 x i8] }
  %.t772 = load { i64, [8 x i8] }, ptr %.t770
  store { i64, [8 x i8] } %.t772, ptr %.t771
  br label %match_merge760
match_merge760:
  %.t773 = phi i1 [ 1, %match_arm761 ], [ 0, %match_arm762 ]
  ret i1 %.t773
}

define i1 @Result$$ptr$Error$$__is_err({ i64, [8 x i8] } %res) {
entry:
  %res.addr = alloca { i64, [8 x i8] }
  store { i64, [8 x i8] } %res, ptr %res.addr
  %.t774 = load { i64, [8 x i8] }, ptr %res.addr
  %.t775 = extractvalue { i64, [8 x i8] } %.t774, 0
  switch i64 %.t775, label %match_arm777 [
    i64 0, label %match_arm777, 
    i64 1, label %match_arm778
  ]
match_arm777:
  %.t779 = alloca { i64, [8 x i8] }
  store { i64, [8 x i8] } %.t774, ptr %.t779
  %.t780 = getelementptr inbounds { i64, [8 x i8] }, ptr %.t779, i32 0, i32 1
  %.t781 = getelementptr inbounds i8, ptr %.t780, i32 0
  %.t782 = alloca ptr
  %.t783 = load ptr, ptr %.t781
  store ptr %.t783, ptr %.t782
  br label %match_merge776
match_arm778:
  %.t784 = alloca { i64, [8 x i8] }
  store { i64, [8 x i8] } %.t774, ptr %.t784
  %.t785 = getelementptr inbounds { i64, [8 x i8] }, ptr %.t784, i32 0, i32 1
  %.t786 = getelementptr inbounds i8, ptr %.t785, i32 0
  %.t787 = alloca { i64, [8 x i8] }
  %.t788 = load { i64, [8 x i8] }, ptr %.t786
  store { i64, [8 x i8] } %.t788, ptr %.t787
  br label %match_merge776
match_merge776:
  %.t789 = phi i1 [ 0, %match_arm777 ], [ 1, %match_arm778 ]
  ret i1 %.t789
}

define ptr @Result$$ptr$Error$$__unwrap_or({ i64, [8 x i8] } %res, ptr %fallback) {
entry:
  %res.addr = alloca { i64, [8 x i8] }
  store { i64, [8 x i8] } %res, ptr %res.addr
  %fallback.addr = alloca ptr
  store ptr %fallback, ptr %fallback.addr
  %.t790 = load { i64, [8 x i8] }, ptr %res.addr
  %.t791 = extractvalue { i64, [8 x i8] } %.t790, 0
  switch i64 %.t791, label %match_arm793 [
    i64 0, label %match_arm793, 
    i64 1, label %match_arm794
  ]
match_arm793:
  %.t795 = alloca { i64, [8 x i8] }
  store { i64, [8 x i8] } %.t790, ptr %.t795
  %.t796 = getelementptr inbounds { i64, [8 x i8] }, ptr %.t795, i32 0, i32 1
  %.t797 = getelementptr inbounds i8, ptr %.t796, i32 0
  %.t798 = alloca ptr
  %.t799 = load ptr, ptr %.t797
  store ptr %.t799, ptr %.t798
  %.t800 = load ptr, ptr %.t798
  %.t801 = ptrtoint ptr %.t800 to i32
  br label %match_merge792
match_arm794:
  %.t802 = alloca { i64, [8 x i8] }
  store { i64, [8 x i8] } %.t790, ptr %.t802
  %.t803 = getelementptr inbounds { i64, [8 x i8] }, ptr %.t802, i32 0, i32 1
  %.t804 = getelementptr inbounds i8, ptr %.t803, i32 0
  %.t805 = alloca { i64, [8 x i8] }
  %.t806 = load { i64, [8 x i8] }, ptr %.t804
  store { i64, [8 x i8] } %.t806, ptr %.t805
  %.t807 = load ptr, ptr %fallback.addr
  %.t808 = ptrtoint ptr %.t807 to i32
  br label %match_merge792
match_merge792:
  %.t809 = phi i32 [ %.t801, %match_arm793 ], [ %.t808, %match_arm794 ]
  %.t810 = inttoptr i32 %.t809 to ptr
  ret ptr %.t810
}

define i1 @Result$$bool$Error$$__is_ok({ i64, [8 x i8] } %res) {
entry:
  %res.addr = alloca { i64, [8 x i8] }
  store { i64, [8 x i8] } %res, ptr %res.addr
  %.t811 = load { i64, [8 x i8] }, ptr %res.addr
  %.t812 = extractvalue { i64, [8 x i8] } %.t811, 0
  switch i64 %.t812, label %match_arm814 [
    i64 0, label %match_arm814, 
    i64 1, label %match_arm815
  ]
match_arm814:
  %.t816 = alloca { i64, [8 x i8] }
  store { i64, [8 x i8] } %.t811, ptr %.t816
  %.t817 = getelementptr inbounds { i64, [8 x i8] }, ptr %.t816, i32 0, i32 1
  %.t818 = getelementptr inbounds i8, ptr %.t817, i32 0
  %.t819 = alloca i1
  %.t820 = load i1, ptr %.t818
  store i1 %.t820, ptr %.t819
  br label %match_merge813
match_arm815:
  %.t821 = alloca { i64, [8 x i8] }
  store { i64, [8 x i8] } %.t811, ptr %.t821
  %.t822 = getelementptr inbounds { i64, [8 x i8] }, ptr %.t821, i32 0, i32 1
  %.t823 = getelementptr inbounds i8, ptr %.t822, i32 0
  %.t824 = alloca { i64, [8 x i8] }
  %.t825 = load { i64, [8 x i8] }, ptr %.t823
  store { i64, [8 x i8] } %.t825, ptr %.t824
  br label %match_merge813
match_merge813:
  %.t826 = phi i1 [ 1, %match_arm814 ], [ 0, %match_arm815 ]
  ret i1 %.t826
}

define i1 @Result$$bool$Error$$__is_err({ i64, [8 x i8] } %res) {
entry:
  %res.addr = alloca { i64, [8 x i8] }
  store { i64, [8 x i8] } %res, ptr %res.addr
  %.t827 = load { i64, [8 x i8] }, ptr %res.addr
  %.t828 = extractvalue { i64, [8 x i8] } %.t827, 0
  switch i64 %.t828, label %match_arm830 [
    i64 0, label %match_arm830, 
    i64 1, label %match_arm831
  ]
match_arm830:
  %.t832 = alloca { i64, [8 x i8] }
  store { i64, [8 x i8] } %.t827, ptr %.t832
  %.t833 = getelementptr inbounds { i64, [8 x i8] }, ptr %.t832, i32 0, i32 1
  %.t834 = getelementptr inbounds i8, ptr %.t833, i32 0
  %.t835 = alloca i1
  %.t836 = load i1, ptr %.t834
  store i1 %.t836, ptr %.t835
  br label %match_merge829
match_arm831:
  %.t837 = alloca { i64, [8 x i8] }
  store { i64, [8 x i8] } %.t827, ptr %.t837
  %.t838 = getelementptr inbounds { i64, [8 x i8] }, ptr %.t837, i32 0, i32 1
  %.t839 = getelementptr inbounds i8, ptr %.t838, i32 0
  %.t840 = alloca { i64, [8 x i8] }
  %.t841 = load { i64, [8 x i8] }, ptr %.t839
  store { i64, [8 x i8] } %.t841, ptr %.t840
  br label %match_merge829
match_merge829:
  %.t842 = phi i1 [ 0, %match_arm830 ], [ 1, %match_arm831 ]
  ret i1 %.t842
}

define i1 @Result$$bool$Error$$__unwrap_or({ i64, [8 x i8] } %res, i1 %fallback) {
entry:
  %res.addr = alloca { i64, [8 x i8] }
  store { i64, [8 x i8] } %res, ptr %res.addr
  %fallback.addr = alloca i1
  store i1 %fallback, ptr %fallback.addr
  %.t843 = load { i64, [8 x i8] }, ptr %res.addr
  %.t844 = extractvalue { i64, [8 x i8] } %.t843, 0
  switch i64 %.t844, label %match_arm846 [
    i64 0, label %match_arm846, 
    i64 1, label %match_arm847
  ]
match_arm846:
  %.t848 = alloca { i64, [8 x i8] }
  store { i64, [8 x i8] } %.t843, ptr %.t848
  %.t849 = getelementptr inbounds { i64, [8 x i8] }, ptr %.t848, i32 0, i32 1
  %.t850 = getelementptr inbounds i8, ptr %.t849, i32 0
  %.t851 = alloca i1
  %.t852 = load i1, ptr %.t850
  store i1 %.t852, ptr %.t851
  %.t853 = load i1, ptr %.t851
  %.t854 = zext i1 %.t853 to i32
  br label %match_merge845
match_arm847:
  %.t855 = alloca { i64, [8 x i8] }
  store { i64, [8 x i8] } %.t843, ptr %.t855
  %.t856 = getelementptr inbounds { i64, [8 x i8] }, ptr %.t855, i32 0, i32 1
  %.t857 = getelementptr inbounds i8, ptr %.t856, i32 0
  %.t858 = alloca { i64, [8 x i8] }
  %.t859 = load { i64, [8 x i8] }, ptr %.t857
  store { i64, [8 x i8] } %.t859, ptr %.t858
  %.t860 = load i1, ptr %fallback.addr
  %.t861 = zext i1 %.t860 to i32
  br label %match_merge845
match_merge845:
  %.t862 = phi i32 [ %.t854, %match_arm846 ], [ %.t861, %match_arm847 ]
  %.t863 = trunc i32 %.t862 to i1
  ret i1 %.t863
}

define i1 @Option$$i64$$__is_some({ i64, [8 x i8] } %opt) {
entry:
  %opt.addr = alloca { i64, [8 x i8] }
  store { i64, [8 x i8] } %opt, ptr %opt.addr
  %.t864 = load { i64, [8 x i8] }, ptr %opt.addr
  %.t865 = extractvalue { i64, [8 x i8] } %.t864, 0
  switch i64 %.t865, label %match_arm867 [
    i64 0, label %match_arm867
  ]
match_arm867:
  %.t869 = alloca { i64, [8 x i8] }
  store { i64, [8 x i8] } %.t864, ptr %.t869
  %.t870 = getelementptr inbounds { i64, [8 x i8] }, ptr %.t869, i32 0, i32 1
  %.t871 = getelementptr inbounds i8, ptr %.t870, i32 0
  %.t872 = alloca i64
  %.t873 = load i64, ptr %.t871
  store i64 %.t873, ptr %.t872
  br label %match_merge866
match_arm868:
  br label %match_merge866
match_merge866:
  %.t874 = phi i1 [ 1, %match_arm867 ], [ 0, %match_arm868 ]
  ret i1 %.t874
}

define i1 @Option$$i64$$__is_none({ i64, [8 x i8] } %opt) {
entry:
  %opt.addr = alloca { i64, [8 x i8] }
  store { i64, [8 x i8] } %opt, ptr %opt.addr
  %.t875 = load { i64, [8 x i8] }, ptr %opt.addr
  %.t876 = extractvalue { i64, [8 x i8] } %.t875, 0
  switch i64 %.t876, label %match_arm878 [
    i64 0, label %match_arm878
  ]
match_arm878:
  %.t880 = alloca { i64, [8 x i8] }
  store { i64, [8 x i8] } %.t875, ptr %.t880
  %.t881 = getelementptr inbounds { i64, [8 x i8] }, ptr %.t880, i32 0, i32 1
  %.t882 = getelementptr inbounds i8, ptr %.t881, i32 0
  %.t883 = alloca i64
  %.t884 = load i64, ptr %.t882
  store i64 %.t884, ptr %.t883
  br label %match_merge877
match_arm879:
  br label %match_merge877
match_merge877:
  %.t885 = phi i1 [ 0, %match_arm878 ], [ 1, %match_arm879 ]
  ret i1 %.t885
}

define i64 @Option$$i64$$__unwrap_or({ i64, [8 x i8] } %opt, i64 %fallback) {
entry:
  %opt.addr = alloca { i64, [8 x i8] }
  store { i64, [8 x i8] } %opt, ptr %opt.addr
  %fallback.addr = alloca i64
  store i64 %fallback, ptr %fallback.addr
  %.t886 = load { i64, [8 x i8] }, ptr %opt.addr
  %.t887 = extractvalue { i64, [8 x i8] } %.t886, 0
  switch i64 %.t887, label %match_arm889 [
    i64 0, label %match_arm889
  ]
match_arm889:
  %.t891 = alloca { i64, [8 x i8] }
  store { i64, [8 x i8] } %.t886, ptr %.t891
  %.t892 = getelementptr inbounds { i64, [8 x i8] }, ptr %.t891, i32 0, i32 1
  %.t893 = getelementptr inbounds i8, ptr %.t892, i32 0
  %.t894 = alloca i64
  %.t895 = load i64, ptr %.t893
  store i64 %.t895, ptr %.t894
  %.t896 = load i64, ptr %.t894
  %.t897 = trunc i64 %.t896 to i32
  br label %match_merge888
match_arm890:
  %.t898 = load i64, ptr %fallback.addr
  %.t899 = trunc i64 %.t898 to i32
  br label %match_merge888
match_merge888:
  %.t900 = phi i32 [ %.t897, %match_arm889 ], [ %.t899, %match_arm890 ]
  %.t901 = sext i32 %.t900 to i64
  ret i64 %.t901
}

define i1 @Option$$u64$$__is_some({ i64, [8 x i8] } %opt) {
entry:
  %opt.addr = alloca { i64, [8 x i8] }
  store { i64, [8 x i8] } %opt, ptr %opt.addr
  %.t902 = load { i64, [8 x i8] }, ptr %opt.addr
  %.t903 = extractvalue { i64, [8 x i8] } %.t902, 0
  switch i64 %.t903, label %match_arm905 [
    i64 0, label %match_arm905
  ]
match_arm905:
  %.t907 = alloca { i64, [8 x i8] }
  store { i64, [8 x i8] } %.t902, ptr %.t907
  %.t908 = getelementptr inbounds { i64, [8 x i8] }, ptr %.t907, i32 0, i32 1
  %.t909 = getelementptr inbounds i8, ptr %.t908, i32 0
  %.t910 = alloca i64
  %.t911 = load i64, ptr %.t909
  store i64 %.t911, ptr %.t910
  br label %match_merge904
match_arm906:
  br label %match_merge904
match_merge904:
  %.t912 = phi i1 [ 1, %match_arm905 ], [ 0, %match_arm906 ]
  ret i1 %.t912
}

define i1 @Option$$u64$$__is_none({ i64, [8 x i8] } %opt) {
entry:
  %opt.addr = alloca { i64, [8 x i8] }
  store { i64, [8 x i8] } %opt, ptr %opt.addr
  %.t913 = load { i64, [8 x i8] }, ptr %opt.addr
  %.t914 = extractvalue { i64, [8 x i8] } %.t913, 0
  switch i64 %.t914, label %match_arm916 [
    i64 0, label %match_arm916
  ]
match_arm916:
  %.t918 = alloca { i64, [8 x i8] }
  store { i64, [8 x i8] } %.t913, ptr %.t918
  %.t919 = getelementptr inbounds { i64, [8 x i8] }, ptr %.t918, i32 0, i32 1
  %.t920 = getelementptr inbounds i8, ptr %.t919, i32 0
  %.t921 = alloca i64
  %.t922 = load i64, ptr %.t920
  store i64 %.t922, ptr %.t921
  br label %match_merge915
match_arm917:
  br label %match_merge915
match_merge915:
  %.t923 = phi i1 [ 0, %match_arm916 ], [ 1, %match_arm917 ]
  ret i1 %.t923
}

define i64 @Option$$u64$$__unwrap_or({ i64, [8 x i8] } %opt, i64 %fallback) {
entry:
  %opt.addr = alloca { i64, [8 x i8] }
  store { i64, [8 x i8] } %opt, ptr %opt.addr
  %fallback.addr = alloca i64
  store i64 %fallback, ptr %fallback.addr
  %.t924 = load { i64, [8 x i8] }, ptr %opt.addr
  %.t925 = extractvalue { i64, [8 x i8] } %.t924, 0
  switch i64 %.t925, label %match_arm927 [
    i64 0, label %match_arm927
  ]
match_arm927:
  %.t929 = alloca { i64, [8 x i8] }
  store { i64, [8 x i8] } %.t924, ptr %.t929
  %.t930 = getelementptr inbounds { i64, [8 x i8] }, ptr %.t929, i32 0, i32 1
  %.t931 = getelementptr inbounds i8, ptr %.t930, i32 0
  %.t932 = alloca i64
  %.t933 = load i64, ptr %.t931
  store i64 %.t933, ptr %.t932
  %.t934 = load i64, ptr %.t932
  %.t935 = trunc i64 %.t934 to i32
  br label %match_merge926
match_arm928:
  %.t936 = load i64, ptr %fallback.addr
  %.t937 = trunc i64 %.t936 to i32
  br label %match_merge926
match_merge926:
  %.t938 = phi i32 [ %.t935, %match_arm927 ], [ %.t937, %match_arm928 ]
  %.t939 = sext i32 %.t938 to i64
  ret i64 %.t939
}

define i1 @Option$$u8$$__is_some({ i64, [1 x i8] } %opt) {
entry:
  %opt.addr = alloca { i64, [1 x i8] }
  store { i64, [1 x i8] } %opt, ptr %opt.addr
  %.t940 = load { i64, [1 x i8] }, ptr %opt.addr
  %.t941 = extractvalue { i64, [1 x i8] } %.t940, 0
  switch i64 %.t941, label %match_arm943 [
    i64 0, label %match_arm943
  ]
match_arm943:
  %.t945 = alloca { i64, [1 x i8] }
  store { i64, [1 x i8] } %.t940, ptr %.t945
  %.t946 = getelementptr inbounds { i64, [1 x i8] }, ptr %.t945, i32 0, i32 1
  %.t947 = getelementptr inbounds i8, ptr %.t946, i32 0
  %.t948 = alloca i8
  %.t949 = load i8, ptr %.t947
  store i8 %.t949, ptr %.t948
  br label %match_merge942
match_arm944:
  br label %match_merge942
match_merge942:
  %.t950 = phi i1 [ 1, %match_arm943 ], [ 0, %match_arm944 ]
  ret i1 %.t950
}

define i1 @Option$$u8$$__is_none({ i64, [1 x i8] } %opt) {
entry:
  %opt.addr = alloca { i64, [1 x i8] }
  store { i64, [1 x i8] } %opt, ptr %opt.addr
  %.t951 = load { i64, [1 x i8] }, ptr %opt.addr
  %.t952 = extractvalue { i64, [1 x i8] } %.t951, 0
  switch i64 %.t952, label %match_arm954 [
    i64 0, label %match_arm954
  ]
match_arm954:
  %.t956 = alloca { i64, [1 x i8] }
  store { i64, [1 x i8] } %.t951, ptr %.t956
  %.t957 = getelementptr inbounds { i64, [1 x i8] }, ptr %.t956, i32 0, i32 1
  %.t958 = getelementptr inbounds i8, ptr %.t957, i32 0
  %.t959 = alloca i8
  %.t960 = load i8, ptr %.t958
  store i8 %.t960, ptr %.t959
  br label %match_merge953
match_arm955:
  br label %match_merge953
match_merge953:
  %.t961 = phi i1 [ 0, %match_arm954 ], [ 1, %match_arm955 ]
  ret i1 %.t961
}

define i8 @Option$$u8$$__unwrap_or({ i64, [1 x i8] } %opt, i8 %fallback) {
entry:
  %opt.addr = alloca { i64, [1 x i8] }
  store { i64, [1 x i8] } %opt, ptr %opt.addr
  %fallback.addr = alloca i8
  store i8 %fallback, ptr %fallback.addr
  %.t962 = load { i64, [1 x i8] }, ptr %opt.addr
  %.t963 = extractvalue { i64, [1 x i8] } %.t962, 0
  switch i64 %.t963, label %match_arm965 [
    i64 0, label %match_arm965
  ]
match_arm965:
  %.t967 = alloca { i64, [1 x i8] }
  store { i64, [1 x i8] } %.t962, ptr %.t967
  %.t968 = getelementptr inbounds { i64, [1 x i8] }, ptr %.t967, i32 0, i32 1
  %.t969 = getelementptr inbounds i8, ptr %.t968, i32 0
  %.t970 = alloca i8
  %.t971 = load i8, ptr %.t969
  store i8 %.t971, ptr %.t970
  %.t972 = load i8, ptr %.t970
  %.t973 = zext i8 %.t972 to i32
  br label %match_merge964
match_arm966:
  %.t974 = load i8, ptr %fallback.addr
  %.t975 = zext i8 %.t974 to i32
  br label %match_merge964
match_merge964:
  %.t976 = phi i32 [ %.t973, %match_arm965 ], [ %.t975, %match_arm966 ]
  %.t977 = trunc i32 %.t976 to i8
  ret i8 %.t977
}

define i1 @Option$$ptr$$__is_some({ i64, [8 x i8] } %opt) {
entry:
  %opt.addr = alloca { i64, [8 x i8] }
  store { i64, [8 x i8] } %opt, ptr %opt.addr
  %.t978 = load { i64, [8 x i8] }, ptr %opt.addr
  %.t979 = extractvalue { i64, [8 x i8] } %.t978, 0
  switch i64 %.t979, label %match_arm981 [
    i64 0, label %match_arm981
  ]
match_arm981:
  %.t983 = alloca { i64, [8 x i8] }
  store { i64, [8 x i8] } %.t978, ptr %.t983
  %.t984 = getelementptr inbounds { i64, [8 x i8] }, ptr %.t983, i32 0, i32 1
  %.t985 = getelementptr inbounds i8, ptr %.t984, i32 0
  %.t986 = alloca ptr
  %.t987 = load ptr, ptr %.t985
  store ptr %.t987, ptr %.t986
  br label %match_merge980
match_arm982:
  br label %match_merge980
match_merge980:
  %.t988 = phi i1 [ 1, %match_arm981 ], [ 0, %match_arm982 ]
  ret i1 %.t988
}

define i1 @Option$$ptr$$__is_none({ i64, [8 x i8] } %opt) {
entry:
  %opt.addr = alloca { i64, [8 x i8] }
  store { i64, [8 x i8] } %opt, ptr %opt.addr
  %.t989 = load { i64, [8 x i8] }, ptr %opt.addr
  %.t990 = extractvalue { i64, [8 x i8] } %.t989, 0
  switch i64 %.t990, label %match_arm992 [
    i64 0, label %match_arm992
  ]
match_arm992:
  %.t994 = alloca { i64, [8 x i8] }
  store { i64, [8 x i8] } %.t989, ptr %.t994
  %.t995 = getelementptr inbounds { i64, [8 x i8] }, ptr %.t994, i32 0, i32 1
  %.t996 = getelementptr inbounds i8, ptr %.t995, i32 0
  %.t997 = alloca ptr
  %.t998 = load ptr, ptr %.t996
  store ptr %.t998, ptr %.t997
  br label %match_merge991
match_arm993:
  br label %match_merge991
match_merge991:
  %.t999 = phi i1 [ 0, %match_arm992 ], [ 1, %match_arm993 ]
  ret i1 %.t999
}

define ptr @Option$$ptr$$__unwrap_or({ i64, [8 x i8] } %opt, ptr %fallback) {
entry:
  %opt.addr = alloca { i64, [8 x i8] }
  store { i64, [8 x i8] } %opt, ptr %opt.addr
  %fallback.addr = alloca ptr
  store ptr %fallback, ptr %fallback.addr
  %.t1000 = load { i64, [8 x i8] }, ptr %opt.addr
  %.t1001 = extractvalue { i64, [8 x i8] } %.t1000, 0
  switch i64 %.t1001, label %match_arm1003 [
    i64 0, label %match_arm1003
  ]
match_arm1003:
  %.t1005 = alloca { i64, [8 x i8] }
  store { i64, [8 x i8] } %.t1000, ptr %.t1005
  %.t1006 = getelementptr inbounds { i64, [8 x i8] }, ptr %.t1005, i32 0, i32 1
  %.t1007 = getelementptr inbounds i8, ptr %.t1006, i32 0
  %.t1008 = alloca ptr
  %.t1009 = load ptr, ptr %.t1007
  store ptr %.t1009, ptr %.t1008
  %.t1010 = load ptr, ptr %.t1008
  %.t1011 = ptrtoint ptr %.t1010 to i32
  br label %match_merge1002
match_arm1004:
  %.t1012 = load ptr, ptr %fallback.addr
  %.t1013 = ptrtoint ptr %.t1012 to i32
  br label %match_merge1002
match_merge1002:
  %.t1014 = phi i32 [ %.t1011, %match_arm1003 ], [ %.t1013, %match_arm1004 ]
  %.t1015 = inttoptr i32 %.t1014 to ptr
  ret ptr %.t1015
}

define i1 @Option$$str$$__is_some({ i64, [8 x i8] } %opt) {
entry:
  %opt.addr = alloca { i64, [8 x i8] }
  store { i64, [8 x i8] } %opt, ptr %opt.addr
  %.t1016 = load { i64, [8 x i8] }, ptr %opt.addr
  %.t1017 = extractvalue { i64, [8 x i8] } %.t1016, 0
  switch i64 %.t1017, label %match_arm1019 [
    i64 0, label %match_arm1019
  ]
match_arm1019:
  %.t1021 = alloca { i64, [8 x i8] }
  store { i64, [8 x i8] } %.t1016, ptr %.t1021
  %.t1022 = getelementptr inbounds { i64, [8 x i8] }, ptr %.t1021, i32 0, i32 1
  %.t1023 = getelementptr inbounds i8, ptr %.t1022, i32 0
  %.t1024 = alloca ptr
  %.t1025 = load ptr, ptr %.t1023
  store ptr %.t1025, ptr %.t1024
  br label %match_merge1018
match_arm1020:
  br label %match_merge1018
match_merge1018:
  %.t1026 = phi i1 [ 1, %match_arm1019 ], [ 0, %match_arm1020 ]
  ret i1 %.t1026
}

define i1 @Option$$str$$__is_none({ i64, [8 x i8] } %opt) {
entry:
  %opt.addr = alloca { i64, [8 x i8] }
  store { i64, [8 x i8] } %opt, ptr %opt.addr
  %.t1027 = load { i64, [8 x i8] }, ptr %opt.addr
  %.t1028 = extractvalue { i64, [8 x i8] } %.t1027, 0
  switch i64 %.t1028, label %match_arm1030 [
    i64 0, label %match_arm1030
  ]
match_arm1030:
  %.t1032 = alloca { i64, [8 x i8] }
  store { i64, [8 x i8] } %.t1027, ptr %.t1032
  %.t1033 = getelementptr inbounds { i64, [8 x i8] }, ptr %.t1032, i32 0, i32 1
  %.t1034 = getelementptr inbounds i8, ptr %.t1033, i32 0
  %.t1035 = alloca ptr
  %.t1036 = load ptr, ptr %.t1034
  store ptr %.t1036, ptr %.t1035
  br label %match_merge1029
match_arm1031:
  br label %match_merge1029
match_merge1029:
  %.t1037 = phi i1 [ 0, %match_arm1030 ], [ 1, %match_arm1031 ]
  ret i1 %.t1037
}

define ptr @Option$$str$$__unwrap_or({ i64, [8 x i8] } %opt, ptr %fallback) {
entry:
  %opt.addr = alloca { i64, [8 x i8] }
  store { i64, [8 x i8] } %opt, ptr %opt.addr
  %fallback.addr = alloca ptr
  store ptr %fallback, ptr %fallback.addr
  %.t1038 = load { i64, [8 x i8] }, ptr %opt.addr
  %.t1039 = extractvalue { i64, [8 x i8] } %.t1038, 0
  switch i64 %.t1039, label %match_arm1041 [
    i64 0, label %match_arm1041
  ]
match_arm1041:
  %.t1043 = alloca { i64, [8 x i8] }
  store { i64, [8 x i8] } %.t1038, ptr %.t1043
  %.t1044 = getelementptr inbounds { i64, [8 x i8] }, ptr %.t1043, i32 0, i32 1
  %.t1045 = getelementptr inbounds i8, ptr %.t1044, i32 0
  %.t1046 = alloca ptr
  %.t1047 = load ptr, ptr %.t1045
  store ptr %.t1047, ptr %.t1046
  %.t1048 = load ptr, ptr %.t1046
  %.t1049 = ptrtoint ptr %.t1048 to i32
  br label %match_merge1040
match_arm1042:
  %.t1050 = load ptr, ptr %fallback.addr
  %.t1051 = ptrtoint ptr %.t1050 to i32
  br label %match_merge1040
match_merge1040:
  %.t1052 = phi i32 [ %.t1049, %match_arm1041 ], [ %.t1051, %match_arm1042 ]
  %.t1053 = inttoptr i32 %.t1052 to ptr
  ret ptr %.t1053
}

define i1 @Option$$bool$$__is_some({ i64, [0 x i8] } %opt) {
entry:
  %opt.addr = alloca { i64, [0 x i8] }
  store { i64, [0 x i8] } %opt, ptr %opt.addr
  %.t1054 = load { i64, [0 x i8] }, ptr %opt.addr
  %.t1055 = extractvalue { i64, [0 x i8] } %.t1054, 0
  switch i64 %.t1055, label %match_arm1057 [
    i64 0, label %match_arm1057
  ]
match_arm1057:
  %.t1059 = alloca { i64, [0 x i8] }
  store { i64, [0 x i8] } %.t1054, ptr %.t1059
  %.t1060 = getelementptr inbounds { i64, [0 x i8] }, ptr %.t1059, i32 0, i32 1
  %.t1061 = getelementptr inbounds i8, ptr %.t1060, i32 0
  %.t1062 = alloca i1
  %.t1063 = load i1, ptr %.t1061
  store i1 %.t1063, ptr %.t1062
  br label %match_merge1056
match_arm1058:
  br label %match_merge1056
match_merge1056:
  %.t1064 = phi i1 [ 1, %match_arm1057 ], [ 0, %match_arm1058 ]
  ret i1 %.t1064
}

define i1 @Option$$bool$$__is_none({ i64, [0 x i8] } %opt) {
entry:
  %opt.addr = alloca { i64, [0 x i8] }
  store { i64, [0 x i8] } %opt, ptr %opt.addr
  %.t1065 = load { i64, [0 x i8] }, ptr %opt.addr
  %.t1066 = extractvalue { i64, [0 x i8] } %.t1065, 0
  switch i64 %.t1066, label %match_arm1068 [
    i64 0, label %match_arm1068
  ]
match_arm1068:
  %.t1070 = alloca { i64, [0 x i8] }
  store { i64, [0 x i8] } %.t1065, ptr %.t1070
  %.t1071 = getelementptr inbounds { i64, [0 x i8] }, ptr %.t1070, i32 0, i32 1
  %.t1072 = getelementptr inbounds i8, ptr %.t1071, i32 0
  %.t1073 = alloca i1
  %.t1074 = load i1, ptr %.t1072
  store i1 %.t1074, ptr %.t1073
  br label %match_merge1067
match_arm1069:
  br label %match_merge1067
match_merge1067:
  %.t1075 = phi i1 [ 0, %match_arm1068 ], [ 1, %match_arm1069 ]
  ret i1 %.t1075
}

define i1 @Option$$bool$$__unwrap_or({ i64, [0 x i8] } %opt, i1 %fallback) {
entry:
  %opt.addr = alloca { i64, [0 x i8] }
  store { i64, [0 x i8] } %opt, ptr %opt.addr
  %fallback.addr = alloca i1
  store i1 %fallback, ptr %fallback.addr
  %.t1076 = load { i64, [0 x i8] }, ptr %opt.addr
  %.t1077 = extractvalue { i64, [0 x i8] } %.t1076, 0
  switch i64 %.t1077, label %match_arm1079 [
    i64 0, label %match_arm1079
  ]
match_arm1079:
  %.t1081 = alloca { i64, [0 x i8] }
  store { i64, [0 x i8] } %.t1076, ptr %.t1081
  %.t1082 = getelementptr inbounds { i64, [0 x i8] }, ptr %.t1081, i32 0, i32 1
  %.t1083 = getelementptr inbounds i8, ptr %.t1082, i32 0
  %.t1084 = alloca i1
  %.t1085 = load i1, ptr %.t1083
  store i1 %.t1085, ptr %.t1084
  %.t1086 = load i1, ptr %.t1084
  %.t1087 = zext i1 %.t1086 to i32
  br label %match_merge1078
match_arm1080:
  %.t1088 = load i1, ptr %fallback.addr
  %.t1089 = zext i1 %.t1088 to i32
  br label %match_merge1078
match_merge1078:
  %.t1090 = phi i32 [ %.t1087, %match_arm1079 ], [ %.t1089, %match_arm1080 ]
  %.t1091 = trunc i32 %.t1090 to i1
  ret i1 %.t1091
}

define ptr @PageAllocator__alloc({ i64 } %self, i64 %num_pages) {
entry:
  %self.addr = alloca { i64 }
  store { i64 } %self, ptr %self.addr
  %num_pages.addr = alloca i64
  store i64 %num_pages, ptr %num_pages.addr
  %.t1092 = load { i64 }, ptr %self.addr
  %pa = alloca { i64 }
  store { i64 } %.t1092, ptr %pa
  %.t1093 = load i64, ptr %num_pages.addr
  %.t1094 = load { i64 }, ptr %pa
  %.t1095 = extractvalue { i64 } %.t1094, 0
  %.t1096 = mul i64 %.t1093, %.t1095
  %total_size = alloca i64
  store i64 %.t1096, ptr %total_size
  %.t1097 = sext i32 0 to i64
  %null_addr = alloca i64
  store i64 %.t1097, ptr %null_addr
  %.t1098 = load i64, ptr %null_addr
  %.t1099 = inttoptr i64 %.t1098 to ptr
  %addr = alloca ptr
  store ptr %.t1099, ptr %addr
  %prot = alloca i32
  store i32 3, ptr %prot
  %flags = alloca i32
  store i32 34, ptr %flags
  %.t1100 = sub i32 0, 1
  %fd = alloca i32
  store i32 %.t1100, ptr %fd
  %.t1101 = sext i32 0 to i64
  %offset = alloca i64
  store i64 %.t1101, ptr %offset
  %.t1102 = load ptr, ptr %addr
  %.t1103 = load i64, ptr %total_size
  %.t1104 = load i32, ptr %prot
  %.t1105 = load i32, ptr %flags
  %.t1106 = load i32, ptr %fd
  %.t1107 = load i64, ptr %offset
  %.t1108 = call ptr @mmap(ptr %.t1102, i64 %.t1103, i32 %.t1104, i32 %.t1105, i32 %.t1106, i64 %.t1107)
  %ptr = alloca ptr
  store ptr %.t1108, ptr %ptr
  %.t1109 = load ptr, ptr %ptr
  ret ptr %.t1109
}

define {  } @PageAllocator__free({ i64 } %self, ptr %ptr, i64 %num_pages) {
entry:
  %self.addr = alloca { i64 }
  store { i64 } %self, ptr %self.addr
  %ptr.addr = alloca ptr
  store ptr %ptr, ptr %ptr.addr
  %num_pages.addr = alloca i64
  store i64 %num_pages, ptr %num_pages.addr
  %.t1110 = load { i64 }, ptr %self.addr
  %pa = alloca { i64 }
  store { i64 } %.t1110, ptr %pa
  %.t1111 = load i64, ptr %num_pages.addr
  %.t1112 = load { i64 }, ptr %pa
  %.t1113 = extractvalue { i64 } %.t1112, 0
  %.t1114 = mul i64 %.t1111, %.t1113
  %total_size = alloca i64
  store i64 %.t1114, ptr %total_size
  %.t1115 = load ptr, ptr %ptr.addr
  %.t1116 = load i64, ptr %total_size
  %.t1117 = call i32 @munmap(ptr %.t1115, i64 %.t1116)
  %result = alloca i32
  store i32 %.t1117, ptr %result
  ret {  } undef
}

define i1 @PageAllocator__realloc({ i64 } %self, ptr %ptr, i64 %new_size) {
entry:
  %self.addr = alloca { i64 }
  store { i64 } %self, ptr %self.addr
  %ptr.addr = alloca ptr
  store ptr %ptr, ptr %ptr.addr
  %new_size.addr = alloca i64
  store i64 %new_size, ptr %new_size.addr
  ret i1 0
}

define ptr @PageAllocator__remap({ i64 } %self, ptr %ptr, i64 %old_pages, i64 %new_pages) {
entry:
  %self.addr = alloca { i64 }
  store { i64 } %self, ptr %self.addr
  %ptr.addr = alloca ptr
  store ptr %ptr, ptr %ptr.addr
  %old_pages.addr = alloca i64
  store i64 %old_pages, ptr %old_pages.addr
  %new_pages.addr = alloca i64
  store i64 %new_pages, ptr %new_pages.addr
  %.t1118 = load { i64 }, ptr %self.addr
  %pa = alloca { i64 }
  store { i64 } %.t1118, ptr %pa
  %.t1119 = load { i64 }, ptr %pa
  %.t1120 = load i64, ptr %new_pages.addr
  %.t1121 = call ptr @PageAllocator__alloc({ i64 } %.t1119, i64 %.t1120)
  %new_ptr = alloca ptr
  store ptr %.t1121, ptr %new_ptr
  %.t1122 = load i64, ptr %old_pages.addr
  %.t1123 = load { i64 }, ptr %pa
  %.t1124 = extractvalue { i64 } %.t1123, 0
  %.t1125 = mul i64 %.t1122, %.t1124
  %old_size = alloca i64
  store i64 %.t1125, ptr %old_size
  %.t1126 = load ptr, ptr %new_ptr
  %.t1127 = load ptr, ptr %ptr.addr
  %.t1128 = load i64, ptr %old_size
  %.t1129 = call {  } @memcpy(ptr %.t1126, ptr %.t1127, i64 %.t1128)
  %.t1130 = load { i64 }, ptr %pa
  %.t1131 = load ptr, ptr %ptr.addr
  %.t1132 = load i64, ptr %old_pages.addr
  %.t1133 = call {  } @PageAllocator__free({ i64 } %.t1130, ptr %.t1131, i64 %.t1132)
  %.t1134 = load ptr, ptr %new_ptr
  ret ptr %.t1134
}

define { ptr, { { i64 }, ptr, i64, i64, i64 } } @Arena__alloc({ { i64 }, ptr, i64, i64, i64 } %self, i64 %size) {
entry:
  %self.addr = alloca { { i64 }, ptr, i64, i64, i64 }
  store { { i64 }, ptr, i64, i64, i64 } %self, ptr %self.addr
  %size.addr = alloca i64
  store i64 %size, ptr %size.addr
  %.t1135 = load { { i64 }, ptr, i64, i64, i64 }, ptr %self.addr
  %arena = alloca { { i64 }, ptr, i64, i64, i64 }
  store { { i64 }, ptr, i64, i64, i64 } %.t1135, ptr %arena
  %.t1136 = load { { i64 }, ptr, i64, i64, i64 }, ptr %arena
  %.t1137 = extractvalue { { i64 }, ptr, i64, i64, i64 } %.t1136, 3
  %.t1138 = load i64, ptr %size.addr
  %.t1139 = add i64 %.t1137, %.t1138
  %new_used = alloca i64
  store i64 %.t1139, ptr %new_used
  %.t1140 = load i64, ptr %new_used
  %.t1141 = load { { i64 }, ptr, i64, i64, i64 }, ptr %arena
  %.t1142 = extractvalue { { i64 }, ptr, i64, i64, i64 } %.t1141, 2
  %.t1143 = icmp ugt i64 %.t1140, %.t1142
  %needs_grow = alloca i1
  store i1 %.t1143, ptr %needs_grow
  %.t1144 = load { { i64 }, ptr, i64, i64, i64 }, ptr %arena
  %arena_updated = alloca { { i64 }, ptr, i64, i64, i64 }
  store { { i64 }, ptr, i64, i64, i64 } %.t1144, ptr %arena_updated
  %.t1148 = load i1, ptr %needs_grow
  br i1 %.t1148, label %if_then1145, label %if_end1147
if_then1145:
  %.t1149 = load { { i64 }, ptr, i64, i64, i64 }, ptr %arena
  %.t1150 = extractvalue { { i64 }, ptr, i64, i64, i64 } %.t1149, 0
  %.t1151 = extractvalue { i64 } %.t1150, 0
  %page_size = alloca i64
  store i64 %.t1151, ptr %page_size
  %.t1152 = load i64, ptr %new_used
  %.t1153 = load i64, ptr %page_size
  %.t1154 = add i64 %.t1152, %.t1153
  %.t1155 = sub i64 %.t1154, 1
  %.t1156 = load i64, ptr %page_size
  %.t1157 = udiv i64 %.t1155, %.t1156
  %total_pages_needed = alloca i64
  store i64 %.t1157, ptr %total_pages_needed
  %.t1158 = load { { i64 }, ptr, i64, i64, i64 }, ptr %arena
  %.t1159 = extractvalue { { i64 }, ptr, i64, i64, i64 } %.t1158, 0
  %.t1160 = load i64, ptr %total_pages_needed
  %.t1161 = call ptr @PageAllocator__alloc({ i64 } %.t1159, i64 %.t1160)
  %new_buffer = alloca ptr
  store ptr %.t1161, ptr %new_buffer
  %.t1162 = load i64, ptr %total_pages_needed
  %.t1163 = load i64, ptr %page_size
  %.t1164 = mul i64 %.t1162, %.t1163
  %new_capacity = alloca i64
  store i64 %.t1164, ptr %new_capacity
  %.t1165 = load { { i64 }, ptr, i64, i64, i64 }, ptr %arena
  %.t1166 = extractvalue { { i64 }, ptr, i64, i64, i64 } %.t1165, 3
  %.t1167 = icmp ugt i64 %.t1166, 0
  %has_old_data = alloca i1
  store i1 %.t1167, ptr %has_old_data
  %.t1171 = load i1, ptr %has_old_data
  br i1 %.t1171, label %if_then1168, label %if_end1170
if_then1168:
  %.t1172 = load ptr, ptr %new_buffer
  %.t1173 = load { { i64 }, ptr, i64, i64, i64 }, ptr %arena
  %.t1174 = extractvalue { { i64 }, ptr, i64, i64, i64 } %.t1173, 1
  %.t1175 = load { { i64 }, ptr, i64, i64, i64 }, ptr %arena
  %.t1176 = extractvalue { { i64 }, ptr, i64, i64, i64 } %.t1175, 3
  %.t1177 = call {  } @memcpy(ptr %.t1172, ptr %.t1174, i64 %.t1176)
  %copy_result = alloca {  }
  store {  } %.t1177, ptr %copy_result
  %.t1178 = load { { i64 }, ptr, i64, i64, i64 }, ptr %arena
  %.t1179 = extractvalue { { i64 }, ptr, i64, i64, i64 } %.t1178, 0
  %.t1180 = load { { i64 }, ptr, i64, i64, i64 }, ptr %arena
  %.t1181 = extractvalue { { i64 }, ptr, i64, i64, i64 } %.t1180, 1
  %.t1182 = load { { i64 }, ptr, i64, i64, i64 }, ptr %arena
  %.t1183 = extractvalue { { i64 }, ptr, i64, i64, i64 } %.t1182, 4
  %.t1184 = call {  } @PageAllocator__free({ i64 } %.t1179, ptr %.t1181, i64 %.t1183)
  %free_result = alloca {  }
  store {  } %.t1184, ptr %free_result
  br label %if_end1170
if_end1170:
  %.t1185 = load { { i64 }, ptr, i64, i64, i64 }, ptr %arena
  %.t1186 = extractvalue { { i64 }, ptr, i64, i64, i64 } %.t1185, 0
  %.t1187 = insertvalue { { i64 }, ptr, i64, i64, i64 } undef, { i64 } %.t1186, 0
  %.t1188 = load ptr, ptr %new_buffer
  %.t1189 = insertvalue { { i64 }, ptr, i64, i64, i64 } %.t1187, ptr %.t1188, 1
  %.t1190 = load i64, ptr %new_capacity
  %.t1191 = insertvalue { { i64 }, ptr, i64, i64, i64 } %.t1189, i64 %.t1190, 2
  %.t1192 = load { { i64 }, ptr, i64, i64, i64 }, ptr %arena
  %.t1193 = extractvalue { { i64 }, ptr, i64, i64, i64 } %.t1192, 3
  %.t1194 = insertvalue { { i64 }, ptr, i64, i64, i64 } %.t1191, i64 %.t1193, 3
  %.t1195 = load i64, ptr %total_pages_needed
  %.t1196 = insertvalue { { i64 }, ptr, i64, i64, i64 } %.t1194, i64 %.t1195, 4
  %new_arena = alloca { { i64 }, ptr, i64, i64, i64 }
  store { { i64 }, ptr, i64, i64, i64 } %.t1196, ptr %new_arena
  %.t1197 = load { { i64 }, ptr, i64, i64, i64 }, ptr %new_arena
  store { { i64 }, ptr, i64, i64, i64 } %.t1197, ptr %arena_updated
  br label %if_end1147
if_end1147:
  %.t1198 = load { { i64 }, ptr, i64, i64, i64 }, ptr %arena_updated
  %.t1199 = extractvalue { { i64 }, ptr, i64, i64, i64 } %.t1198, 1
  %.t1200 = load { { i64 }, ptr, i64, i64, i64 }, ptr %arena_updated
  %.t1201 = extractvalue { { i64 }, ptr, i64, i64, i64 } %.t1200, 3
  %.t1202 = getelementptr i64, ptr %.t1199, i64 %.t1201
  %alloc_ptr = alloca ptr
  store ptr %.t1202, ptr %alloc_ptr
  %.t1203 = load { { i64 }, ptr, i64, i64, i64 }, ptr %arena_updated
  %.t1204 = extractvalue { { i64 }, ptr, i64, i64, i64 } %.t1203, 0
  %.t1205 = insertvalue { { i64 }, ptr, i64, i64, i64 } undef, { i64 } %.t1204, 0
  %.t1206 = load { { i64 }, ptr, i64, i64, i64 }, ptr %arena_updated
  %.t1207 = extractvalue { { i64 }, ptr, i64, i64, i64 } %.t1206, 1
  %.t1208 = insertvalue { { i64 }, ptr, i64, i64, i64 } %.t1205, ptr %.t1207, 1
  %.t1209 = load { { i64 }, ptr, i64, i64, i64 }, ptr %arena_updated
  %.t1210 = extractvalue { { i64 }, ptr, i64, i64, i64 } %.t1209, 2
  %.t1211 = insertvalue { { i64 }, ptr, i64, i64, i64 } %.t1208, i64 %.t1210, 2
  %.t1212 = load i64, ptr %new_used
  %.t1213 = insertvalue { { i64 }, ptr, i64, i64, i64 } %.t1211, i64 %.t1212, 3
  %.t1214 = load { { i64 }, ptr, i64, i64, i64 }, ptr %arena_updated
  %.t1215 = extractvalue { { i64 }, ptr, i64, i64, i64 } %.t1214, 4
  %.t1216 = insertvalue { { i64 }, ptr, i64, i64, i64 } %.t1213, i64 %.t1215, 4
  %final_arena = alloca { { i64 }, ptr, i64, i64, i64 }
  store { { i64 }, ptr, i64, i64, i64 } %.t1216, ptr %final_arena
  %.t1217 = load ptr, ptr %alloc_ptr
  %.t1218 = insertvalue { ptr, { { i64 }, ptr, i64, i64, i64 } } undef, ptr %.t1217, 0
  %.t1219 = load { { i64 }, ptr, i64, i64, i64 }, ptr %final_arena
  %.t1220 = insertvalue { ptr, { { i64 }, ptr, i64, i64, i64 } } %.t1218, { { i64 }, ptr, i64, i64, i64 } %.t1219, 1
  ret { ptr, { { i64 }, ptr, i64, i64, i64 } } %.t1220
}

define {  } @Arena__free({ { i64 }, ptr, i64, i64, i64 } %self, ptr %ptr) {
entry:
  %self.addr = alloca { { i64 }, ptr, i64, i64, i64 }
  store { { i64 }, ptr, i64, i64, i64 } %self, ptr %self.addr
  %ptr.addr = alloca ptr
  store ptr %ptr, ptr %ptr.addr
  ret {  } undef
}

define i1 @Arena__realloc({ { i64 }, ptr, i64, i64, i64 } %self, ptr %ptr, i64 %new_size) {
entry:
  %self.addr = alloca { { i64 }, ptr, i64, i64, i64 }
  store { { i64 }, ptr, i64, i64, i64 } %self, ptr %self.addr
  %ptr.addr = alloca ptr
  store ptr %ptr, ptr %ptr.addr
  %new_size.addr = alloca i64
  store i64 %new_size, ptr %new_size.addr
  ret i1 0
}

define ptr @Arena__remap({ { i64 }, ptr, i64, i64, i64 } %self, ptr %old_ptr, i64 %new_size) {
entry:
  %self.addr = alloca { { i64 }, ptr, i64, i64, i64 }
  store { { i64 }, ptr, i64, i64, i64 } %self, ptr %self.addr
  %old_ptr.addr = alloca ptr
  store ptr %old_ptr, ptr %old_ptr.addr
  %new_size.addr = alloca i64
  store i64 %new_size, ptr %new_size.addr
  %.t1221 = load { { i64 }, ptr, i64, i64, i64 }, ptr %self.addr
  %arena = alloca { { i64 }, ptr, i64, i64, i64 }
  store { { i64 }, ptr, i64, i64, i64 } %.t1221, ptr %arena
  %.t1222 = load { { i64 }, ptr, i64, i64, i64 }, ptr %arena
  %.t1223 = extractvalue { { i64 }, ptr, i64, i64, i64 } %.t1222, 1
  ret ptr %.t1223
}

define { ptr, { { i64 }, ptr, ptr, ptr } } @GeneralPurposeAllocator__alloc({ { i64 }, ptr, ptr, ptr } %self, i64 %size) {
entry:
  %self.addr = alloca { { i64 }, ptr, ptr, ptr }
  store { { i64 }, ptr, ptr, ptr } %self, ptr %self.addr
  %size.addr = alloca i64
  store i64 %size, ptr %size.addr
  %.t1224 = load { { i64 }, ptr, ptr, ptr }, ptr %self.addr
  %gpa = alloca { { i64 }, ptr, ptr, ptr }
  store { { i64 }, ptr, ptr, ptr } %.t1224, ptr %gpa
  %.t1225 = sext i32 16 to i64
  %header_size = alloca i64
  store i64 %.t1225, ptr %header_size
  %.t1226 = load i64, ptr %size.addr
  %.t1227 = load i64, ptr %header_size
  %.t1228 = add i64 %.t1226, %.t1227
  %total_size = alloca i64
  store i64 %.t1228, ptr %total_size
  %.t1229 = sext i32 0 to i64
  %null_addr = alloca i64
  store i64 %.t1229, ptr %null_addr
  %.t1230 = load i64, ptr %null_addr
  %.t1231 = inttoptr i64 %.t1230 to ptr
  %null_ptr = alloca ptr
  store ptr %.t1231, ptr %null_ptr
  %.t1232 = load { { i64 }, ptr, ptr, ptr }, ptr %gpa
  %.t1233 = extractvalue { { i64 }, ptr, ptr, ptr } %.t1232, 1
  %current = alloca ptr
  store ptr %.t1233, ptr %current
  %.t1234 = load ptr, ptr %null_ptr
  %prev = alloca ptr
  store ptr %.t1234, ptr %prev
  %.t1235 = load ptr, ptr %null_ptr
  %found_block = alloca ptr
  store ptr %.t1235, ptr %found_block
  %.t1236 = load ptr, ptr %null_ptr
  %found_prev = alloca ptr
  store ptr %.t1236, ptr %found_prev
  br label %while_header1237
while_header1237:
  %.t1240 = load ptr, ptr %current
  %.t1241 = load ptr, ptr %null_ptr
  %.t1242 = icmp ne ptr %.t1240, %.t1241
  br i1 %.t1242, label %while_body1238, label %while_exit1239
while_body1238:
  %.t1243 = load ptr, ptr %current
  %.t1244 = load i64, ptr %.t1243
  %block_size = alloca i64
  store i64 %.t1244, ptr %block_size
  %.t1245 = load i64, ptr %block_size
  %.t1246 = load i64, ptr %total_size
  %.t1247 = icmp uge i64 %.t1245, %.t1246
  %fits = alloca i1
  store i1 %.t1247, ptr %fits
  %.t1248 = load i1, ptr %fits
  br i1 %.t1248, label %then1249, label %else1250
then1249:
  %.t1252 = load ptr, ptr %current
  store ptr %.t1252, ptr %found_block
  %.t1253 = load ptr, ptr %prev
  store ptr %.t1253, ptr %found_prev
  %.t1254 = load ptr, ptr %null_ptr
  store ptr %.t1254, ptr %current
  br label %merge1251
else1250:
  %.t1255 = load ptr, ptr %current
  %.t1256 = getelementptr i64, ptr %.t1255, i64 8
  %current_next_offset = alloca ptr
  store ptr %.t1256, ptr %current_next_offset
  %.t1257 = load ptr, ptr %current_next_offset
  %.t1258 = load ptr, ptr %.t1257
  %next = alloca ptr
  store ptr %.t1258, ptr %next
  %.t1259 = load ptr, ptr %current
  store ptr %.t1259, ptr %prev
  %.t1260 = load ptr, ptr %next
  store ptr %.t1260, ptr %current
  br label %merge1251
merge1251:
  %.t1261 = phi i32 [ 0, %then1249 ], [ 0, %else1250 ]
  br label %while_header1237
while_exit1239:
  %.t1262 = load ptr, ptr %found_block
  %.t1263 = load ptr, ptr %null_ptr
  %.t1264 = icmp ne ptr %.t1262, %.t1263
  br i1 %.t1264, label %then1265, label %else1266
then1265:
  %.t1268 = load ptr, ptr %found_block
  %.t1269 = getelementptr i64, ptr %.t1268, i64 8
  %next_offset2 = alloca ptr
  store ptr %.t1269, ptr %next_offset2
  %.t1270 = load ptr, ptr %next_offset2
  %.t1271 = load ptr, ptr %.t1270
  %next_block = alloca ptr
  store ptr %.t1271, ptr %next_block
  %.t1272 = load ptr, ptr %found_prev
  %.t1273 = load ptr, ptr %null_ptr
  %.t1274 = icmp eq ptr %.t1272, %.t1273
  br i1 %.t1274, label %then1275, label %else1276
then1275:
  %.t1278 = load ptr, ptr %next_block
  br label %merge1277
else1276:
  %.t1279 = load ptr, ptr %found_prev
  %.t1280 = getelementptr i64, ptr %.t1279, i64 8
  %prev_next_offset = alloca ptr
  store ptr %.t1280, ptr %prev_next_offset
  %.t1281 = load ptr, ptr %prev_next_offset
  %.t1282 = load ptr, ptr %next_block
  %.t1283 = ptrtoint ptr %.t1282 to i64
  store i64 %.t1283, ptr %.t1281
  %_w3 = alloca {  }
  store {  } undef, ptr %_w3
  %.t1284 = load { { i64 }, ptr, ptr, ptr }, ptr %gpa
  %.t1285 = extractvalue { { i64 }, ptr, ptr, ptr } %.t1284, 1
  br label %merge1277
merge1277:
  %.t1286 = phi ptr [ %.t1278, %then1275 ], [ %.t1285, %else1276 ]
  %new_free_list = alloca ptr
  store ptr %.t1286, ptr %new_free_list
  %.t1287 = load { { i64 }, ptr, ptr, ptr }, ptr %gpa
  %.t1288 = extractvalue { { i64 }, ptr, ptr, ptr } %.t1287, 0
  %.t1289 = insertvalue { { i64 }, ptr, ptr, ptr } undef, { i64 } %.t1288, 0
  %.t1290 = load ptr, ptr %new_free_list
  %.t1291 = insertvalue { { i64 }, ptr, ptr, ptr } %.t1289, ptr %.t1290, 1
  %.t1292 = load { { i64 }, ptr, ptr, ptr }, ptr %gpa
  %.t1293 = extractvalue { { i64 }, ptr, ptr, ptr } %.t1292, 2
  %.t1294 = insertvalue { { i64 }, ptr, ptr, ptr } %.t1291, ptr %.t1293, 2
  %.t1295 = load { { i64 }, ptr, ptr, ptr }, ptr %gpa
  %.t1296 = extractvalue { { i64 }, ptr, ptr, ptr } %.t1295, 3
  %.t1297 = insertvalue { { i64 }, ptr, ptr, ptr } %.t1294, ptr %.t1296, 3
  %updated_gpa = alloca { { i64 }, ptr, ptr, ptr }
  store { { i64 }, ptr, ptr, ptr } %.t1297, ptr %updated_gpa
  %.t1298 = load { { i64 }, ptr, ptr, ptr }, ptr %updated_gpa
  store { { i64 }, ptr, ptr, ptr } %.t1298, ptr %gpa
  %.t1299 = load ptr, ptr %found_block
  %.t1300 = load i64, ptr %header_size
  %.t1301 = getelementptr i64, ptr %.t1299, i64 %.t1300
  br label %merge1267
else1266:
  %.t1302 = load { { i64 }, ptr, ptr, ptr }, ptr %gpa
  %.t1303 = extractvalue { { i64 }, ptr, ptr, ptr } %.t1302, 0
  %.t1304 = extractvalue { i64 } %.t1303, 0
  %page_size = alloca i64
  store i64 %.t1304, ptr %page_size
  %.t1305 = load i64, ptr %total_size
  %.t1306 = load i64, ptr %page_size
  %.t1307 = add i64 %.t1305, %.t1306
  %.t1308 = sub i64 %.t1307, 1
  %.t1309 = load i64, ptr %page_size
  %.t1310 = udiv i64 %.t1308, %.t1309
  %pages_needed = alloca i64
  store i64 %.t1310, ptr %pages_needed
  %.t1311 = load { { i64 }, ptr, ptr, ptr }, ptr %gpa
  %.t1312 = extractvalue { { i64 }, ptr, ptr, ptr } %.t1311, 0
  %.t1313 = load i64, ptr %pages_needed
  %.t1314 = call ptr @PageAllocator__alloc({ i64 } %.t1312, i64 %.t1313)
  %new_block = alloca ptr
  store ptr %.t1314, ptr %new_block
  %.t1315 = load ptr, ptr %new_block
  %.t1316 = load i64, ptr %total_size
  store i64 %.t1316, ptr %.t1315
  %_w1 = alloca {  }
  store {  } undef, ptr %_w1
  %.t1317 = load ptr, ptr %new_block
  %.t1318 = getelementptr i64, ptr %.t1317, i64 8
  %new_block_next_offset = alloca ptr
  store ptr %.t1318, ptr %new_block_next_offset
  %.t1319 = load ptr, ptr %new_block_next_offset
  %.t1320 = load ptr, ptr %null_ptr
  %.t1321 = ptrtoint ptr %.t1320 to i64
  store i64 %.t1321, ptr %.t1319
  %_w2 = alloca {  }
  store {  } undef, ptr %_w2
  %.t1322 = load ptr, ptr %new_block
  %.t1323 = load i64, ptr %header_size
  %.t1324 = getelementptr i64, ptr %.t1322, i64 %.t1323
  br label %merge1267
merge1267:
  %.t1325 = phi ptr [ %.t1301, %merge1277 ], [ %.t1324, %else1266 ]
  %allocated_ptr = alloca ptr
  store ptr %.t1325, ptr %allocated_ptr
  %.t1326 = load { { i64 }, ptr, ptr, ptr }, ptr %gpa
  %final_gpa = alloca { { i64 }, ptr, ptr, ptr }
  store { { i64 }, ptr, ptr, ptr } %.t1326, ptr %final_gpa
  %.t1327 = load ptr, ptr %allocated_ptr
  %.t1328 = insertvalue { ptr, { { i64 }, ptr, ptr, ptr } } undef, ptr %.t1327, 0
  %.t1329 = load { { i64 }, ptr, ptr, ptr }, ptr %final_gpa
  %.t1330 = insertvalue { ptr, { { i64 }, ptr, ptr, ptr } } %.t1328, { { i64 }, ptr, ptr, ptr } %.t1329, 1
  ret { ptr, { { i64 }, ptr, ptr, ptr } } %.t1330
}

define { { i64 }, ptr, ptr, ptr } @GeneralPurposeAllocator__free({ { i64 }, ptr, ptr, ptr } %self, ptr %ptr) {
entry:
  %self.addr = alloca { { i64 }, ptr, ptr, ptr }
  store { { i64 }, ptr, ptr, ptr } %self, ptr %self.addr
  %ptr.addr = alloca ptr
  store ptr %ptr, ptr %ptr.addr
  %.t1331 = load { { i64 }, ptr, ptr, ptr }, ptr %self.addr
  %gpa = alloca { { i64 }, ptr, ptr, ptr }
  store { { i64 }, ptr, ptr, ptr } %.t1331, ptr %gpa
  %.t1332 = sext i32 16 to i64
  %header_size = alloca i64
  store i64 %.t1332, ptr %header_size
  %.t1333 = load ptr, ptr %ptr.addr
  %.t1334 = ptrtoint ptr %.t1333 to i64
  %ptr_as_int = alloca i64
  store i64 %.t1334, ptr %ptr_as_int
  %.t1335 = sext i32 16 to i64
  %header_size_as_int = alloca i64
  store i64 %.t1335, ptr %header_size_as_int
  %.t1336 = load i64, ptr %ptr_as_int
  %.t1337 = load i64, ptr %header_size_as_int
  %.t1338 = sub i64 %.t1336, %.t1337
  %block_start_int = alloca i64
  store i64 %.t1338, ptr %block_start_int
  %.t1339 = load i64, ptr %block_start_int
  %.t1340 = inttoptr i64 %.t1339 to ptr
  %block_start = alloca ptr
  store ptr %.t1340, ptr %block_start
  %.t1341 = load { { i64 }, ptr, ptr, ptr }, ptr %gpa
  %.t1342 = extractvalue { { i64 }, ptr, ptr, ptr } %.t1341, 1
  %old_head = alloca ptr
  store ptr %.t1342, ptr %old_head
  %.t1343 = load ptr, ptr %block_start
  %.t1344 = getelementptr i64, ptr %.t1343, i64 8
  %next_offset = alloca ptr
  store ptr %.t1344, ptr %next_offset
  %.t1345 = load ptr, ptr %next_offset
  %.t1346 = load ptr, ptr %old_head
  %.t1347 = ptrtoint ptr %.t1346 to i64
  store i64 %.t1347, ptr %.t1345
  %_w1 = alloca {  }
  store {  } undef, ptr %_w1
  %.t1348 = load { { i64 }, ptr, ptr, ptr }, ptr %gpa
  %.t1349 = extractvalue { { i64 }, ptr, ptr, ptr } %.t1348, 0
  %.t1350 = insertvalue { { i64 }, ptr, ptr, ptr } undef, { i64 } %.t1349, 0
  %.t1351 = load ptr, ptr %block_start
  %.t1352 = insertvalue { { i64 }, ptr, ptr, ptr } %.t1350, ptr %.t1351, 1
  %.t1353 = load { { i64 }, ptr, ptr, ptr }, ptr %gpa
  %.t1354 = extractvalue { { i64 }, ptr, ptr, ptr } %.t1353, 2
  %.t1355 = insertvalue { { i64 }, ptr, ptr, ptr } %.t1352, ptr %.t1354, 2
  %.t1356 = load { { i64 }, ptr, ptr, ptr }, ptr %gpa
  %.t1357 = extractvalue { { i64 }, ptr, ptr, ptr } %.t1356, 3
  %.t1358 = insertvalue { { i64 }, ptr, ptr, ptr } %.t1355, ptr %.t1357, 3
  ret { { i64 }, ptr, ptr, ptr } %.t1358
}

define i1 @GeneralPurposeAllocator__realloc({ { i64 }, ptr, ptr, ptr } %self, ptr %ptr, i64 %new_size) {
entry:
  %self.addr = alloca { { i64 }, ptr, ptr, ptr }
  store { { i64 }, ptr, ptr, ptr } %self, ptr %self.addr
  %ptr.addr = alloca ptr
  store ptr %ptr, ptr %ptr.addr
  %new_size.addr = alloca i64
  store i64 %new_size, ptr %new_size.addr
  ret i1 0
}

define { ptr, { { i64 }, ptr, ptr, ptr } } @GeneralPurposeAllocator__remap({ { i64 }, ptr, ptr, ptr } %self, ptr %ptr, i64 %new_size) {
entry:
  %self.addr = alloca { { i64 }, ptr, ptr, ptr }
  store { { i64 }, ptr, ptr, ptr } %self, ptr %self.addr
  %ptr.addr = alloca ptr
  store ptr %ptr, ptr %ptr.addr
  %new_size.addr = alloca i64
  store i64 %new_size, ptr %new_size.addr
  %.t1359 = load { { i64 }, ptr, ptr, ptr }, ptr %self.addr
  %gpa = alloca { { i64 }, ptr, ptr, ptr }
  store { { i64 }, ptr, ptr, ptr } %.t1359, ptr %gpa
  %.t1360 = load ptr, ptr %ptr.addr
  %.t1361 = ptrtoint ptr %.t1360 to i64
  %ptr_as_int = alloca i64
  store i64 %.t1361, ptr %ptr_as_int
  %.t1362 = sext i32 16 to i64
  %header_size = alloca i64
  store i64 %.t1362, ptr %header_size
  %.t1363 = load i64, ptr %ptr_as_int
  %.t1364 = load i64, ptr %header_size
  %.t1365 = sub i64 %.t1363, %.t1364
  %block_start_int = alloca i64
  store i64 %.t1365, ptr %block_start_int
  %.t1366 = load i64, ptr %block_start_int
  %.t1367 = inttoptr i64 %.t1366 to ptr
  %block_start = alloca ptr
  store ptr %.t1367, ptr %block_start
  %.t1368 = load ptr, ptr %block_start
  %.t1369 = load i64, ptr %.t1368
  %old_total_size = alloca i64
  store i64 %.t1369, ptr %old_total_size
  %.t1370 = load i64, ptr %old_total_size
  %.t1371 = load i64, ptr %header_size
  %.t1372 = sub i64 %.t1370, %.t1371
  %old_size = alloca i64
  store i64 %.t1372, ptr %old_size
  %.t1373 = load { { i64 }, ptr, ptr, ptr }, ptr %gpa
  %.t1374 = load i64, ptr %new_size.addr
  %.t1375 = call { ptr, { { i64 }, ptr, ptr, ptr } } @GeneralPurposeAllocator__alloc({ { i64 }, ptr, ptr, ptr } %.t1373, i64 %.t1374)
  %alloc_result = alloca { ptr, { { i64 }, ptr, ptr, ptr } }
  store { ptr, { { i64 }, ptr, ptr, ptr } } %.t1375, ptr %alloc_result
  %.t1376 = load { ptr, { { i64 }, ptr, ptr, ptr } }, ptr %alloc_result
  %.t1377 = extractvalue { ptr, { { i64 }, ptr, ptr, ptr } } %.t1376, 0
  %new_ptr = alloca ptr
  store ptr %.t1377, ptr %new_ptr
  %.t1378 = load { ptr, { { i64 }, ptr, ptr, ptr } }, ptr %alloc_result
  %.t1379 = extractvalue { ptr, { { i64 }, ptr, ptr, ptr } } %.t1378, 1
  %gpa2 = alloca { { i64 }, ptr, ptr, ptr }
  store { { i64 }, ptr, ptr, ptr } %.t1379, ptr %gpa2
  %.t1380 = load i64, ptr %old_size
  %.t1381 = load i64, ptr %new_size.addr
  %.t1382 = icmp ult i64 %.t1380, %.t1381
  br i1 %.t1382, label %then1383, label %else1384
then1383:
  %.t1386 = load i64, ptr %old_size
  br label %merge1385
else1384:
  %.t1387 = load i64, ptr %new_size.addr
  br label %merge1385
merge1385:
  %.t1388 = phi i64 [ %.t1386, %then1383 ], [ %.t1387, %else1384 ]
  %copy_size = alloca i64
  store i64 %.t1388, ptr %copy_size
  %.t1389 = load ptr, ptr %new_ptr
  %.t1390 = load ptr, ptr %ptr.addr
  %.t1391 = load i64, ptr %copy_size
  %.t1392 = call {  } @memcpy(ptr %.t1389, ptr %.t1390, i64 %.t1391)
  %.t1393 = load { { i64 }, ptr, ptr, ptr }, ptr %gpa2
  %.t1394 = load ptr, ptr %ptr.addr
  %.t1395 = call { { i64 }, ptr, ptr, ptr } @GeneralPurposeAllocator__free({ { i64 }, ptr, ptr, ptr } %.t1393, ptr %.t1394)
  %gpa3 = alloca { { i64 }, ptr, ptr, ptr }
  store { { i64 }, ptr, ptr, ptr } %.t1395, ptr %gpa3
  %.t1396 = load ptr, ptr %new_ptr
  %.t1397 = insertvalue { ptr, { { i64 }, ptr, ptr, ptr } } undef, ptr %.t1396, 0
  %.t1398 = load { { i64 }, ptr, ptr, ptr }, ptr %gpa3
  %.t1399 = insertvalue { ptr, { { i64 }, ptr, ptr, ptr } } %.t1397, { { i64 }, ptr, ptr, ptr } %.t1398, 1
  ret { ptr, { { i64 }, ptr, ptr, ptr } } %.t1399
}

define i64 @str__len(ptr %self) {
entry:
  %self.addr = alloca ptr
  store ptr %self, ptr %self.addr
  %.t1400 = sext i32 0 to i64
  %length = alloca i64
  store i64 %.t1400, ptr %length
  %.t1401 = load ptr, ptr %self.addr
  %p = alloca ptr
  store ptr %.t1401, ptr %p
  br label %while_header1402
while_header1402:
  %.t1405 = load ptr, ptr %p
  %.t1406 = load i8, ptr %.t1405
  %.t1407 = zext i8 %.t1406 to i64
  %.t1408 = icmp ne i64 %.t1407, 0
  br i1 %.t1408, label %while_body1403, label %while_exit1404
while_body1403:
  %.t1409 = load i64, ptr %length
  %.t1410 = add i64 %.t1409, 1
  store i64 %.t1410, ptr %length
  %.t1411 = load ptr, ptr %p
  %.t1412 = getelementptr i64, ptr %.t1411, i64 1
  store ptr %.t1412, ptr %p
  br label %while_header1402
while_exit1404:
  %.t1413 = load i64, ptr %length
  ret i64 %.t1413
}

define i8 @str__at(ptr %self, i64 %index) {
entry:
  %self.addr = alloca ptr
  store ptr %self, ptr %self.addr
  %index.addr = alloca i64
  store i64 %index, ptr %index.addr
  %.t1414 = load ptr, ptr %self.addr
  %p = alloca ptr
  store ptr %.t1414, ptr %p
  %.t1415 = load ptr, ptr %p
  %.t1416 = load i64, ptr %index.addr
  %.t1417 = getelementptr i64, ptr %.t1415, i64 %.t1416
  store ptr %.t1417, ptr %p
  %.t1418 = load ptr, ptr %p
  %.t1419 = load i8, ptr %.t1418
  %.t1420 = zext i8 %.t1419 to i64
  %.t1421 = trunc i64 %.t1420 to i8
  ret i8 %.t1421
}

define { ptr, { { i64 }, ptr, i64, i64, i64 } } @str__slice(ptr %self, i64 %start, i64 %end, { { i64 }, ptr, i64, i64, i64 } %arena) {
entry:
  %self.addr = alloca ptr
  store ptr %self, ptr %self.addr
  %start.addr = alloca i64
  store i64 %start, ptr %start.addr
  %end.addr = alloca i64
  store i64 %end, ptr %end.addr
  %arena.addr = alloca { { i64 }, ptr, i64, i64, i64 }
  store { { i64 }, ptr, i64, i64, i64 } %arena, ptr %arena.addr
  %.t1422 = load i64, ptr %end.addr
  %.t1423 = load i64, ptr %start.addr
  %.t1424 = sub i64 %.t1422, %.t1423
  %slice_len = alloca i64
  store i64 %.t1424, ptr %slice_len
  %.t1425 = load i64, ptr %slice_len
  %.t1426 = icmp sle i64 %.t1425, 0
  %is_invalid = alloca i1
  store i1 %.t1426, ptr %is_invalid
  %.t1427 = load i1, ptr %is_invalid
  br i1 %.t1427, label %then1428, label %else1429
then1428:
  %.t1431 = insertvalue { ptr, { { i64 }, ptr, i64, i64, i64 } } undef, ptr getelementptr inbounds ([1 x i8], ptr @.str.12, i32 0, i32 0), 0
  %.t1432 = load { { i64 }, ptr, i64, i64, i64 }, ptr %arena.addr
  %.t1433 = insertvalue { ptr, { { i64 }, ptr, i64, i64, i64 } } %.t1431, { { i64 }, ptr, i64, i64, i64 } %.t1432, 1
  br label %merge1430
else1429:
  %.t1434 = load i64, ptr %slice_len
  %.t1435 = inttoptr i64 %.t1434 to ptr
  %len_ptr = alloca ptr
  store ptr %.t1435, ptr %len_ptr
  %.t1436 = load ptr, ptr %len_ptr
  %.t1437 = ptrtoint ptr %.t1436 to i64
  %len_u64 = alloca i64
  store i64 %.t1437, ptr %len_u64
  %.t1438 = load i64, ptr %len_u64
  %.t1439 = add i64 %.t1438, 1
  %alloc_size = alloca i64
  store i64 %.t1439, ptr %alloc_size
  %.t1440 = load { { i64 }, ptr, i64, i64, i64 }, ptr %arena.addr
  %.t1441 = load i64, ptr %alloc_size
  %.t1442 = call { ptr, { { i64 }, ptr, i64, i64, i64 } } @Arena__alloc({ { i64 }, ptr, i64, i64, i64 } %.t1440, i64 %.t1441)
  %alloc_result = alloca { ptr, { { i64 }, ptr, i64, i64, i64 } }
  store { ptr, { { i64 }, ptr, i64, i64, i64 } } %.t1442, ptr %alloc_result
  %.t1443 = load { ptr, { { i64 }, ptr, i64, i64, i64 } }, ptr %alloc_result
  %.t1444 = extractvalue { ptr, { { i64 }, ptr, i64, i64, i64 } } %.t1443, 0
  %buffer = alloca ptr
  store ptr %.t1444, ptr %buffer
  %.t1445 = load { ptr, { { i64 }, ptr, i64, i64, i64 } }, ptr %alloc_result
  %.t1446 = extractvalue { ptr, { { i64 }, ptr, i64, i64, i64 } } %.t1445, 1
  %arena2 = alloca { { i64 }, ptr, i64, i64, i64 }
  store { { i64 }, ptr, i64, i64, i64 } %.t1446, ptr %arena2
  %.t1447 = load ptr, ptr %self.addr
  %.t1448 = load i64, ptr %start.addr
  %.t1449 = getelementptr i64, ptr %.t1447, i64 %.t1448
  %src = alloca ptr
  store ptr %.t1449, ptr %src
  %.t1450 = load ptr, ptr %buffer
  %.t1451 = load ptr, ptr %src
  %.t1452 = load i64, ptr %len_u64
  %.t1453 = call {  } @memcpy(ptr %.t1450, ptr %.t1451, i64 %.t1452)
  %.t1454 = load ptr, ptr %buffer
  %.t1455 = load i64, ptr %len_u64
  %.t1456 = getelementptr i64, ptr %.t1454, i64 %.t1455
  %null_pos = alloca ptr
  store ptr %.t1456, ptr %null_pos
  %.t1457 = trunc i32 0 to i8
  %zero = alloca i8
  store i8 %.t1457, ptr %zero
  %.t1458 = load ptr, ptr %null_pos
  %.t1459 = load i8, ptr %zero
  store i8 %.t1459, ptr %.t1458
  %.t1460 = load ptr, ptr %buffer
  %.t1461 = insertvalue { ptr, { { i64 }, ptr, i64, i64, i64 } } undef, ptr %.t1460, 0
  %.t1462 = load { { i64 }, ptr, i64, i64, i64 }, ptr %arena2
  %.t1463 = insertvalue { ptr, { { i64 }, ptr, i64, i64, i64 } } %.t1461, { { i64 }, ptr, i64, i64, i64 } %.t1462, 1
  br label %merge1430
merge1430:
  %.t1464 = phi { ptr, { { i64 }, ptr, i64, i64, i64 } } [ %.t1433, %then1428 ], [ %.t1463, %else1429 ]
  ret { ptr, { { i64 }, ptr, i64, i64, i64 } } %.t1464
}

define i1 @str__is_empty(ptr %self) {
entry:
  %self.addr = alloca ptr
  store ptr %self, ptr %self.addr
  %.t1465 = load ptr, ptr %self.addr
  %p = alloca ptr
  store ptr %.t1465, ptr %p
  %.t1466 = load ptr, ptr %p
  %.t1467 = load i8, ptr %.t1466
  %.t1468 = zext i8 %.t1467 to i64
  %.t1469 = trunc i64 %.t1468 to i8
  %first = alloca i8
  store i8 %.t1469, ptr %first
  %.t1470 = load i8, ptr %first
  %.t1471 = icmp eq i8 %.t1470, 0
  ret i1 %.t1471
}

define i1 @str__equals(ptr %self, ptr %other) {
entry:
  %self.addr = alloca ptr
  store ptr %self, ptr %self.addr
  %other.addr = alloca ptr
  store ptr %other, ptr %other.addr
  %.t1472 = load ptr, ptr %self.addr
  %p1 = alloca ptr
  store ptr %.t1472, ptr %p1
  %.t1473 = load ptr, ptr %other.addr
  %p2 = alloca ptr
  store ptr %.t1473, ptr %p2
  %continue = alloca i1
  store i1 1, ptr %continue
  %result = alloca i1
  store i1 1, ptr %result
  br label %while_header1474
while_header1474:
  %.t1477 = load i1, ptr %continue
  br i1 %.t1477, label %while_body1475, label %while_exit1476
while_body1475:
  %.t1478 = load ptr, ptr %p1
  %.t1479 = load i8, ptr %.t1478
  %.t1480 = zext i8 %.t1479 to i64
  %.t1481 = trunc i64 %.t1480 to i8
  %c1 = alloca i8
  store i8 %.t1481, ptr %c1
  %.t1482 = load ptr, ptr %p2
  %.t1483 = load i8, ptr %.t1482
  %.t1484 = zext i8 %.t1483 to i64
  %.t1485 = trunc i64 %.t1484 to i8
  %c2 = alloca i8
  store i8 %.t1485, ptr %c2
  %.t1486 = load i8, ptr %c1
  %.t1487 = load i8, ptr %c2
  %.t1488 = icmp ne i8 %.t1486, %.t1487
  br i1 %.t1488, label %then1489, label %else1490
then1489:
  br label %merge1491
else1490:
  %.t1492 = load i8, ptr %c1
  %.t1493 = icmp eq i8 %.t1492, 0
  br i1 %.t1493, label %then1494, label %else1495
then1494:
  br label %merge1496
else1495:
  br label %merge1496
merge1496:
  %.t1497 = phi i1 [ 1, %then1494 ], [ 1, %else1495 ]
  br label %merge1491
merge1491:
  %.t1498 = phi i1 [ 0, %then1489 ], [ %.t1497, %merge1496 ]
  store i1 %.t1498, ptr %result
  %.t1499 = load i8, ptr %c1
  %.t1500 = load i8, ptr %c2
  %.t1501 = icmp ne i8 %.t1499, %.t1500
  br i1 %.t1501, label %then1502, label %else1503
then1502:
  br label %merge1504
else1503:
  %.t1505 = load i8, ptr %c1
  %.t1506 = icmp eq i8 %.t1505, 0
  br i1 %.t1506, label %then1507, label %else1508
then1507:
  br label %merge1509
else1508:
  br label %merge1509
merge1509:
  %.t1510 = phi i1 [ 0, %then1507 ], [ 1, %else1508 ]
  br label %merge1504
merge1504:
  %.t1511 = phi i1 [ 0, %then1502 ], [ %.t1510, %merge1509 ]
  store i1 %.t1511, ptr %continue
  %.t1512 = load ptr, ptr %p1
  %.t1513 = getelementptr i64, ptr %.t1512, i64 1
  store ptr %.t1513, ptr %p1
  %.t1514 = load ptr, ptr %p2
  %.t1515 = getelementptr i64, ptr %.t1514, i64 1
  store ptr %.t1515, ptr %p2
  br label %while_header1474
while_exit1476:
  %.t1516 = load i1, ptr %result
  ret i1 %.t1516
}

define i1 @str__starts_with(ptr %self, ptr %prefix) {
entry:
  %self.addr = alloca ptr
  store ptr %self, ptr %self.addr
  %prefix.addr = alloca ptr
  store ptr %prefix, ptr %prefix.addr
  %.t1517 = load ptr, ptr %self.addr
  %p_str = alloca ptr
  store ptr %.t1517, ptr %p_str
  %.t1518 = load ptr, ptr %prefix.addr
  %p_prefix = alloca ptr
  store ptr %.t1518, ptr %p_prefix
  %result = alloca i1
  store i1 1, ptr %result
  %continue = alloca i1
  store i1 1, ptr %continue
  br label %while_header1519
while_header1519:
  %.t1522 = load i1, ptr %continue
  br i1 %.t1522, label %while_body1520, label %while_exit1521
while_body1520:
  %.t1523 = load ptr, ptr %p_prefix
  %.t1524 = load i8, ptr %.t1523
  %.t1525 = zext i8 %.t1524 to i64
  %.t1526 = trunc i64 %.t1525 to i8
  %prefix_char = alloca i8
  store i8 %.t1526, ptr %prefix_char
  %.t1527 = load i8, ptr %prefix_char
  %.t1528 = icmp ne i8 %.t1527, 0
  br i1 %.t1528, label %then1529, label %else1530
then1529:
  br label %merge1531
else1530:
  br label %merge1531
merge1531:
  %.t1532 = phi i1 [ 1, %then1529 ], [ 0, %else1530 ]
  store i1 %.t1532, ptr %continue
  %.t1533 = load i1, ptr %continue
  br i1 %.t1533, label %then1534, label %else1535
then1534:
  %.t1537 = load ptr, ptr %p_str
  %.t1538 = load i8, ptr %.t1537
  %.t1539 = zext i8 %.t1538 to i64
  br label %merge1536
else1535:
  br label %merge1536
merge1536:
  %.t1540 = phi i64 [ %.t1539, %then1534 ], [ 0, %else1535 ]
  %.t1541 = trunc i64 %.t1540 to i8
  %c_str = alloca i8
  store i8 %.t1541, ptr %c_str
  %.t1542 = load i1, ptr %continue
  br i1 %.t1542, label %then1543, label %else1544
then1543:
  %.t1546 = load ptr, ptr %p_prefix
  %.t1547 = load i8, ptr %.t1546
  %.t1548 = zext i8 %.t1547 to i64
  br label %merge1545
else1544:
  br label %merge1545
merge1545:
  %.t1549 = phi i64 [ %.t1548, %then1543 ], [ 0, %else1544 ]
  %.t1550 = trunc i64 %.t1549 to i8
  %c_prefix = alloca i8
  store i8 %.t1550, ptr %c_prefix
  %.t1551 = load i1, ptr %continue
  br i1 %.t1551, label %then1552, label %else1553
then1552:
  %.t1555 = load i8, ptr %c_str
  %.t1556 = load i8, ptr %c_prefix
  %.t1557 = icmp ne i8 %.t1555, %.t1556
  br i1 %.t1557, label %then1558, label %else1559
then1558:
  br label %merge1560
else1559:
  br label %merge1560
merge1560:
  %.t1561 = phi i1 [ 0, %then1558 ], [ 1, %else1559 ]
  br label %merge1554
else1553:
  %.t1562 = load i1, ptr %result
  br label %merge1554
merge1554:
  %.t1563 = phi i1 [ %.t1561, %merge1560 ], [ %.t1562, %else1553 ]
  store i1 %.t1563, ptr %result
  %.t1564 = load i1, ptr %result
  br i1 %.t1564, label %then1565, label %else1566
then1565:
  %.t1568 = load i1, ptr %continue
  br label %merge1567
else1566:
  br label %merge1567
merge1567:
  %.t1569 = phi i1 [ %.t1568, %then1565 ], [ 0, %else1566 ]
  store i1 %.t1569, ptr %continue
  %.t1570 = load ptr, ptr %p_str
  %.t1571 = getelementptr i64, ptr %.t1570, i64 1
  store ptr %.t1571, ptr %p_str
  %.t1572 = load ptr, ptr %p_prefix
  %.t1573 = getelementptr i64, ptr %.t1572, i64 1
  store ptr %.t1573, ptr %p_prefix
  br label %while_header1519
while_exit1521:
  %.t1574 = load i1, ptr %result
  ret i1 %.t1574
}

define i1 @str__ends_with(ptr %self, ptr %suffix) {
entry:
  %self.addr = alloca ptr
  store ptr %self, ptr %self.addr
  %suffix.addr = alloca ptr
  store ptr %suffix, ptr %suffix.addr
  %.t1575 = load ptr, ptr %self.addr
  %.t1576 = call i64 @str__len(ptr %.t1575)
  %self_len = alloca i64
  store i64 %.t1576, ptr %self_len
  %.t1577 = load ptr, ptr %suffix.addr
  %.t1578 = call i64 @str__len(ptr %.t1577)
  %suffix_len = alloca i64
  store i64 %.t1578, ptr %suffix_len
  %.t1579 = load i64, ptr %suffix_len
  %.t1580 = load i64, ptr %self_len
  %.t1581 = icmp sgt i64 %.t1579, %.t1580
  br i1 %.t1581, label %then1582, label %else1583
then1582:
  br label %merge1584
else1583:
  br label %merge1584
merge1584:
  %.t1585 = phi i1 [ 0, %then1582 ], [ 1, %else1583 ]
  %result = alloca i1
  store i1 %.t1585, ptr %result
  %.t1586 = load i1, ptr %result
  br i1 %.t1586, label %then1587, label %else1588
then1587:
  %.t1590 = load i64, ptr %self_len
  %.t1591 = load i64, ptr %suffix_len
  %.t1592 = sub i64 %.t1590, %.t1591
  %start_pos = alloca i64
  store i64 %.t1592, ptr %start_pos
  %.t1593 = load ptr, ptr %self.addr
  %p_str = alloca ptr
  store ptr %.t1593, ptr %p_str
  %.t1594 = load ptr, ptr %p_str
  %.t1595 = load i64, ptr %start_pos
  %.t1596 = getelementptr i64, ptr %.t1594, i64 %.t1595
  store ptr %.t1596, ptr %p_str
  %.t1597 = load ptr, ptr %suffix.addr
  %p_suffix = alloca ptr
  store ptr %.t1597, ptr %p_suffix
  %continue = alloca i1
  store i1 1, ptr %continue
  %is_match = alloca i1
  store i1 1, ptr %is_match
  br label %while_header1598
while_header1598:
  %.t1601 = load i1, ptr %continue
  br i1 %.t1601, label %while_body1599, label %while_exit1600
while_body1599:
  %.t1602 = load ptr, ptr %p_suffix
  %.t1603 = load i8, ptr %.t1602
  %.t1604 = zext i8 %.t1603 to i64
  %.t1605 = trunc i64 %.t1604 to i8
  %suffix_char = alloca i8
  store i8 %.t1605, ptr %suffix_char
  %.t1606 = load i8, ptr %suffix_char
  %.t1607 = icmp ne i8 %.t1606, 0
  br i1 %.t1607, label %then1608, label %else1609
then1608:
  br label %merge1610
else1609:
  br label %merge1610
merge1610:
  %.t1611 = phi i1 [ 1, %then1608 ], [ 0, %else1609 ]
  store i1 %.t1611, ptr %continue
  %.t1612 = load i1, ptr %continue
  br i1 %.t1612, label %then1613, label %else1614
then1613:
  %.t1616 = load ptr, ptr %p_str
  %.t1617 = load i8, ptr %.t1616
  %.t1618 = zext i8 %.t1617 to i64
  br label %merge1615
else1614:
  br label %merge1615
merge1615:
  %.t1619 = phi i64 [ %.t1618, %then1613 ], [ 0, %else1614 ]
  %.t1620 = trunc i64 %.t1619 to i8
  %c_str = alloca i8
  store i8 %.t1620, ptr %c_str
  %.t1621 = load i1, ptr %continue
  br i1 %.t1621, label %then1622, label %else1623
then1622:
  %.t1625 = load ptr, ptr %p_suffix
  %.t1626 = load i8, ptr %.t1625
  %.t1627 = zext i8 %.t1626 to i64
  br label %merge1624
else1623:
  br label %merge1624
merge1624:
  %.t1628 = phi i64 [ %.t1627, %then1622 ], [ 0, %else1623 ]
  %.t1629 = trunc i64 %.t1628 to i8
  %c_suffix = alloca i8
  store i8 %.t1629, ptr %c_suffix
  %.t1630 = load i1, ptr %continue
  br i1 %.t1630, label %then1631, label %else1632
then1631:
  %.t1634 = load i8, ptr %c_str
  %.t1635 = load i8, ptr %c_suffix
  %.t1636 = icmp ne i8 %.t1634, %.t1635
  br i1 %.t1636, label %then1637, label %else1638
then1637:
  br label %merge1639
else1638:
  br label %merge1639
merge1639:
  %.t1640 = phi i1 [ 0, %then1637 ], [ 1, %else1638 ]
  br label %merge1633
else1632:
  %.t1641 = load i1, ptr %is_match
  br label %merge1633
merge1633:
  %.t1642 = phi i1 [ %.t1640, %merge1639 ], [ %.t1641, %else1632 ]
  store i1 %.t1642, ptr %is_match
  %.t1643 = load i1, ptr %is_match
  br i1 %.t1643, label %then1644, label %else1645
then1644:
  %.t1647 = load i1, ptr %continue
  br label %merge1646
else1645:
  br label %merge1646
merge1646:
  %.t1648 = phi i1 [ %.t1647, %then1644 ], [ 0, %else1645 ]
  store i1 %.t1648, ptr %continue
  %.t1649 = load ptr, ptr %p_str
  %.t1650 = getelementptr i64, ptr %.t1649, i64 1
  store ptr %.t1650, ptr %p_str
  %.t1651 = load ptr, ptr %p_suffix
  %.t1652 = getelementptr i64, ptr %.t1651, i64 1
  store ptr %.t1652, ptr %p_suffix
  br label %while_header1598
while_exit1600:
  %.t1653 = load i1, ptr %is_match
  br label %merge1589
else1588:
  br label %merge1589
merge1589:
  %.t1654 = phi i1 [ %.t1653, %while_exit1600 ], [ 0, %else1588 ]
  store i1 %.t1654, ptr %result
  %.t1655 = load i1, ptr %result
  ret i1 %.t1655
}

define i64 @str__find(ptr %self, ptr %needle) {
entry:
  %self.addr = alloca ptr
  store ptr %self, ptr %self.addr
  %needle.addr = alloca ptr
  store ptr %needle, ptr %needle.addr
  %.t1656 = load ptr, ptr %self.addr
  %p_haystack = alloca ptr
  store ptr %.t1656, ptr %p_haystack
  %.t1657 = sext i32 0 to i64
  %pos = alloca i64
  store i64 %.t1657, ptr %pos
  %found = alloca i1
  store i1 0, ptr %found
  %.t1658 = sub i32 0, 1
  %.t1659 = sext i32 %.t1658 to i64
  %result = alloca i64
  store i64 %.t1659, ptr %result
  %continue = alloca i1
  store i1 1, ptr %continue
  %.t1660 = load ptr, ptr %self.addr
  %p_check = alloca ptr
  store ptr %.t1660, ptr %p_check
  %.t1661 = load ptr, ptr %needle.addr
  %p_needle = alloca ptr
  store ptr %.t1661, ptr %p_needle
  %matches = alloca i1
  store i1 1, ptr %matches
  %inner_continue = alloca i1
  store i1 1, ptr %inner_continue
  br label %while_header1662
while_header1662:
  %.t1665 = load i1, ptr %continue
  br i1 %.t1665, label %while_body1663, label %while_exit1664
while_body1663:
  %.t1666 = load ptr, ptr %p_haystack
  %.t1667 = load i8, ptr %.t1666
  %.t1668 = zext i8 %.t1667 to i64
  %.t1669 = trunc i64 %.t1668 to i8
  %haystack_char = alloca i8
  store i8 %.t1669, ptr %haystack_char
  %.t1670 = load i8, ptr %haystack_char
  %.t1671 = icmp ne i8 %.t1670, 0
  br i1 %.t1671, label %then1672, label %else1673
then1672:
  br label %merge1674
else1673:
  br label %merge1674
merge1674:
  %.t1675 = phi i1 [ 1, %then1672 ], [ 0, %else1673 ]
  store i1 %.t1675, ptr %continue
  %.t1676 = load i1, ptr %continue
  br i1 %.t1676, label %then1677, label %else1678
then1677:
  %.t1680 = load ptr, ptr %p_haystack
  br label %merge1679
else1678:
  %.t1681 = load ptr, ptr %p_check
  br label %merge1679
merge1679:
  %.t1682 = phi ptr [ %.t1680, %then1677 ], [ %.t1681, %else1678 ]
  store ptr %.t1682, ptr %p_check
  %.t1683 = load i1, ptr %continue
  br i1 %.t1683, label %then1684, label %else1685
then1684:
  %.t1687 = load ptr, ptr %needle.addr
  br label %merge1686
else1685:
  %.t1688 = load ptr, ptr %p_needle
  br label %merge1686
merge1686:
  %.t1689 = phi ptr [ %.t1687, %then1684 ], [ %.t1688, %else1685 ]
  store ptr %.t1689, ptr %p_needle
  %.t1690 = load i1, ptr %continue
  br i1 %.t1690, label %then1691, label %else1692
then1691:
  br label %merge1693
else1692:
  %.t1694 = load i1, ptr %matches
  br label %merge1693
merge1693:
  %.t1695 = phi i1 [ 1, %then1691 ], [ %.t1694, %else1692 ]
  store i1 %.t1695, ptr %matches
  %.t1696 = load i1, ptr %continue
  br i1 %.t1696, label %then1697, label %else1698
then1697:
  br label %merge1699
else1698:
  %.t1700 = load i1, ptr %inner_continue
  br label %merge1699
merge1699:
  %.t1701 = phi i1 [ 1, %then1697 ], [ %.t1700, %else1698 ]
  store i1 %.t1701, ptr %inner_continue
  br label %while_header1702
while_header1702:
  %.t1705 = load i1, ptr %inner_continue
  br i1 %.t1705, label %while_body1703, label %while_exit1704
while_body1703:
  %.t1706 = load ptr, ptr %p_needle
  %.t1707 = load i8, ptr %.t1706
  %.t1708 = zext i8 %.t1707 to i64
  %.t1709 = trunc i64 %.t1708 to i8
  %needle_char = alloca i8
  store i8 %.t1709, ptr %needle_char
  %.t1710 = load i8, ptr %needle_char
  %.t1711 = icmp ne i8 %.t1710, 0
  br i1 %.t1711, label %then1712, label %else1713
then1712:
  br label %merge1714
else1713:
  br label %merge1714
merge1714:
  %.t1715 = phi i1 [ 1, %then1712 ], [ 0, %else1713 ]
  store i1 %.t1715, ptr %inner_continue
  %.t1716 = load i1, ptr %inner_continue
  br i1 %.t1716, label %then1717, label %else1718
then1717:
  %.t1720 = load ptr, ptr %p_check
  %.t1721 = load i8, ptr %.t1720
  %.t1722 = zext i8 %.t1721 to i64
  br label %merge1719
else1718:
  br label %merge1719
merge1719:
  %.t1723 = phi i64 [ %.t1722, %then1717 ], [ 0, %else1718 ]
  %.t1724 = trunc i64 %.t1723 to i8
  %c_check = alloca i8
  store i8 %.t1724, ptr %c_check
  %.t1725 = load i1, ptr %inner_continue
  br i1 %.t1725, label %then1726, label %else1727
then1726:
  %.t1729 = load ptr, ptr %p_needle
  %.t1730 = load i8, ptr %.t1729
  %.t1731 = zext i8 %.t1730 to i64
  br label %merge1728
else1727:
  br label %merge1728
merge1728:
  %.t1732 = phi i64 [ %.t1731, %then1726 ], [ 0, %else1727 ]
  %.t1733 = trunc i64 %.t1732 to i8
  %c_needle = alloca i8
  store i8 %.t1733, ptr %c_needle
  %.t1734 = load i1, ptr %inner_continue
  br i1 %.t1734, label %then1735, label %else1736
then1735:
  %.t1738 = load i8, ptr %c_check
  %.t1739 = load i8, ptr %c_needle
  %.t1740 = icmp ne i8 %.t1738, %.t1739
  br i1 %.t1740, label %then1741, label %else1742
then1741:
  br label %merge1743
else1742:
  %.t1744 = load i1, ptr %matches
  br label %merge1743
merge1743:
  %.t1745 = phi i1 [ 0, %then1741 ], [ %.t1744, %else1742 ]
  br label %merge1737
else1736:
  %.t1746 = load i1, ptr %matches
  br label %merge1737
merge1737:
  %.t1747 = phi i1 [ %.t1745, %merge1743 ], [ %.t1746, %else1736 ]
  store i1 %.t1747, ptr %matches
  %.t1748 = load ptr, ptr %p_check
  %.t1749 = getelementptr i64, ptr %.t1748, i64 1
  store ptr %.t1749, ptr %p_check
  %.t1750 = load ptr, ptr %p_needle
  %.t1751 = getelementptr i64, ptr %.t1750, i64 1
  store ptr %.t1751, ptr %p_needle
  br label %while_header1702
while_exit1704:
  %.t1752 = load i1, ptr %continue
  br i1 %.t1752, label %then1753, label %else1754
then1753:
  %.t1756 = load i1, ptr %matches
  br label %merge1755
else1754:
  %.t1757 = load i1, ptr %found
  br label %merge1755
merge1755:
  %.t1758 = phi i1 [ %.t1756, %then1753 ], [ %.t1757, %else1754 ]
  store i1 %.t1758, ptr %found
  %.t1759 = load i1, ptr %found
  br i1 %.t1759, label %then1760, label %else1761
then1760:
  %.t1763 = load i64, ptr %pos
  br label %merge1762
else1761:
  %.t1764 = load i64, ptr %result
  br label %merge1762
merge1762:
  %.t1765 = phi i64 [ %.t1763, %then1760 ], [ %.t1764, %else1761 ]
  store i64 %.t1765, ptr %result
  %.t1766 = load i1, ptr %found
  br i1 %.t1766, label %then1767, label %else1768
then1767:
  br label %merge1769
else1768:
  %.t1770 = load i1, ptr %continue
  br label %merge1769
merge1769:
  %.t1771 = phi i1 [ 0, %then1767 ], [ %.t1770, %else1768 ]
  store i1 %.t1771, ptr %continue
  %.t1772 = load ptr, ptr %p_haystack
  %.t1773 = getelementptr i64, ptr %.t1772, i64 1
  store ptr %.t1773, ptr %p_haystack
  %.t1774 = load i64, ptr %pos
  %.t1775 = add i64 %.t1774, 1
  store i64 %.t1775, ptr %pos
  br label %while_header1662
while_exit1664:
  %.t1776 = load i64, ptr %result
  ret i64 %.t1776
}

define i1 @str__contains(ptr %self, ptr %needle) {
entry:
  %self.addr = alloca ptr
  store ptr %self, ptr %self.addr
  %needle.addr = alloca ptr
  store ptr %needle, ptr %needle.addr
  %.t1777 = load ptr, ptr %self.addr
  %.t1778 = load ptr, ptr %needle.addr
  %.t1779 = call i64 @str__find(ptr %.t1777, ptr %.t1778)
  %pos = alloca i64
  store i64 %.t1779, ptr %pos
  %.t1780 = load i64, ptr %pos
  %.t1781 = icmp sge i64 %.t1780, 0
  ret i1 %.t1781
}

define i8 @str__char_at(ptr %self, i64 %index) {
entry:
  %self.addr = alloca ptr
  store ptr %self, ptr %self.addr
  %index.addr = alloca i64
  store i64 %index, ptr %index.addr
  %.t1782 = load ptr, ptr %self.addr
  %.t1783 = load i64, ptr %index.addr
  %.t1784 = call i8 @str__at(ptr %.t1782, i64 %.t1783)
  ret i8 %.t1784
}

; Entry point - replaces crt1.o
declare i32 @__libc_start_main(ptr, i32, ptr, ptr, ptr, ptr, ptr)

define void @_start() {
entry:
  ; Get argc from stack (first thing on stack at program start)
  %argc_ptr = call ptr asm "mov %rsp, $0", "=r"()
  %argc = load i32, ptr %argc_ptr
  ; argv is at rsp + 8
  %argv = getelementptr i8, ptr %argc_ptr, i64 8
  ; Call __libc_start_main(main, argc, argv, 0, 0, 0, 0)
  %ret = call i32 @__libc_start_main(ptr @main, i32 %argc, ptr %argv, ptr null, ptr null, ptr null, ptr null)
  ; __libc_start_main never returns, but just in case
  unreachable
}
