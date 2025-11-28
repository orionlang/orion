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
if_else357:
  %.t380 = load { ptr, i64, i64, i64 }, ptr %lexer.addr
  ret { ptr, i64, i64, i64 } %.t380
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
  %.t397 = insertvalue { { i64, [8 x i8] }, ptr, i64, i64 } %.t396, ptr getelementptr inbounds ([1 x i8], ptr @.str.18151106360299273605, i32 0, i32 0), 1
  %.t398 = load i64, ptr %line
  %.t399 = insertvalue { { i64, [8 x i8] }, ptr, i64, i64 } %.t397, i64 %.t398, 2
  %.t400 = load i64, ptr %column
  %.t401 = insertvalue { { i64, [8 x i8] }, ptr, i64, i64 } %.t399, i64 %.t400, 3
  %.t402 = insertvalue { { { i64, [8 x i8] }, ptr, i64, i64 }, { ptr, i64, i64, i64 } } undef, { { i64, [8 x i8] }, ptr, i64, i64 } %.t401, 0
  %.t403 = load { ptr, i64, i64, i64 }, ptr %l
  %.t404 = insertvalue { { { i64, [8 x i8] }, ptr, i64, i64 }, { ptr, i64, i64, i64 } } %.t402, { ptr, i64, i64, i64 } %.t403, 1
  ret { { { i64, [8 x i8] }, ptr, i64, i64 }, { ptr, i64, i64, i64 } } %.t404
if_end392:
  %.t408 = load i8, ptr %c
  %.t409 = icmp eq i8 %.t408, 40
  br i1 %.t409, label %if_then405, label %if_else406
if_then405:
  %.t410 = insertvalue { i64, [8 x i8] } undef, i64 45, 0
  %.t411 = insertvalue { { i64, [8 x i8] }, ptr, i64, i64 } undef, { i64, [8 x i8] } %.t410, 0
  %.t412 = insertvalue { { i64, [8 x i8] }, ptr, i64, i64 } %.t411, ptr getelementptr inbounds ([2 x i8], ptr @.str.15882864822835482160, i32 0, i32 0), 1
  %.t413 = load i64, ptr %line
  %.t414 = insertvalue { { i64, [8 x i8] }, ptr, i64, i64 } %.t412, i64 %.t413, 2
  %.t415 = load i64, ptr %column
  %.t416 = insertvalue { { i64, [8 x i8] }, ptr, i64, i64 } %.t414, i64 %.t415, 3
  %.t417 = insertvalue { { { i64, [8 x i8] }, ptr, i64, i64 }, { ptr, i64, i64, i64 } } undef, { { i64, [8 x i8] }, ptr, i64, i64 } %.t416, 0
  %.t418 = load { ptr, i64, i64, i64 }, ptr %l
  %.t419 = call { ptr, i64, i64, i64 } @advance({ ptr, i64, i64, i64 } %.t418)
  %.t420 = insertvalue { { { i64, [8 x i8] }, ptr, i64, i64 }, { ptr, i64, i64, i64 } } %.t417, { ptr, i64, i64, i64 } %.t419, 1
  ret { { { i64, [8 x i8] }, ptr, i64, i64 }, { ptr, i64, i64, i64 } } %.t420
if_else406:
  %.t424 = load i8, ptr %c
  %.t425 = icmp eq i8 %.t424, 41
  br i1 %.t425, label %if_then421, label %if_else422
if_then421:
  %.t426 = insertvalue { i64, [8 x i8] } undef, i64 46, 0
  %.t427 = insertvalue { { i64, [8 x i8] }, ptr, i64, i64 } undef, { i64, [8 x i8] } %.t426, 0
  %.t428 = insertvalue { { i64, [8 x i8] }, ptr, i64, i64 } %.t427, ptr getelementptr inbounds ([2 x i8], ptr @.str.2200420175901286535, i32 0, i32 0), 1
  %.t429 = load i64, ptr %line
  %.t430 = insertvalue { { i64, [8 x i8] }, ptr, i64, i64 } %.t428, i64 %.t429, 2
  %.t431 = load i64, ptr %column
  %.t432 = insertvalue { { i64, [8 x i8] }, ptr, i64, i64 } %.t430, i64 %.t431, 3
  %.t433 = insertvalue { { { i64, [8 x i8] }, ptr, i64, i64 }, { ptr, i64, i64, i64 } } undef, { { i64, [8 x i8] }, ptr, i64, i64 } %.t432, 0
  %.t434 = load { ptr, i64, i64, i64 }, ptr %l
  %.t435 = call { ptr, i64, i64, i64 } @advance({ ptr, i64, i64, i64 } %.t434)
  %.t436 = insertvalue { { { i64, [8 x i8] }, ptr, i64, i64 }, { ptr, i64, i64, i64 } } %.t433, { ptr, i64, i64, i64 } %.t435, 1
  ret { { { i64, [8 x i8] }, ptr, i64, i64 }, { ptr, i64, i64, i64 } } %.t436
if_else422:
  %.t440 = load i8, ptr %c
  %.t441 = icmp eq i8 %.t440, 91
  br i1 %.t441, label %if_then437, label %if_else438
if_then437:
  %.t442 = insertvalue { i64, [8 x i8] } undef, i64 47, 0
  %.t443 = insertvalue { { i64, [8 x i8] }, ptr, i64, i64 } undef, { i64, [8 x i8] } %.t442, 0
  %.t444 = insertvalue { { i64, [8 x i8] }, ptr, i64, i64 } %.t443, ptr getelementptr inbounds ([2 x i8], ptr @.str.16740139525474306003, i32 0, i32 0), 1
  %.t445 = load i64, ptr %line
  %.t446 = insertvalue { { i64, [8 x i8] }, ptr, i64, i64 } %.t444, i64 %.t445, 2
  %.t447 = load i64, ptr %column
  %.t448 = insertvalue { { i64, [8 x i8] }, ptr, i64, i64 } %.t446, i64 %.t447, 3
  %.t449 = insertvalue { { { i64, [8 x i8] }, ptr, i64, i64 }, { ptr, i64, i64, i64 } } undef, { { i64, [8 x i8] }, ptr, i64, i64 } %.t448, 0
  %.t450 = load { ptr, i64, i64, i64 }, ptr %l
  %.t451 = call { ptr, i64, i64, i64 } @advance({ ptr, i64, i64, i64 } %.t450)
  %.t452 = insertvalue { { { i64, [8 x i8] }, ptr, i64, i64 }, { ptr, i64, i64, i64 } } %.t449, { ptr, i64, i64, i64 } %.t451, 1
  ret { { { i64, [8 x i8] }, ptr, i64, i64 }, { ptr, i64, i64, i64 } } %.t452
if_else438:
  %.t456 = load i8, ptr %c
  %.t457 = icmp eq i8 %.t456, 93
  br i1 %.t457, label %if_then453, label %if_else454
if_then453:
  %.t458 = insertvalue { i64, [8 x i8] } undef, i64 48, 0
  %.t459 = insertvalue { { i64, [8 x i8] }, ptr, i64, i64 } undef, { i64, [8 x i8] } %.t458, 0
  %.t460 = insertvalue { { i64, [8 x i8] }, ptr, i64, i64 } %.t459, ptr getelementptr inbounds ([2 x i8], ptr @.str.810667871647353583, i32 0, i32 0), 1
  %.t461 = load i64, ptr %line
  %.t462 = insertvalue { { i64, [8 x i8] }, ptr, i64, i64 } %.t460, i64 %.t461, 2
  %.t463 = load i64, ptr %column
  %.t464 = insertvalue { { i64, [8 x i8] }, ptr, i64, i64 } %.t462, i64 %.t463, 3
  %.t465 = insertvalue { { { i64, [8 x i8] }, ptr, i64, i64 }, { ptr, i64, i64, i64 } } undef, { { i64, [8 x i8] }, ptr, i64, i64 } %.t464, 0
  %.t466 = load { ptr, i64, i64, i64 }, ptr %l
  %.t467 = call { ptr, i64, i64, i64 } @advance({ ptr, i64, i64, i64 } %.t466)
  %.t468 = insertvalue { { { i64, [8 x i8] }, ptr, i64, i64 }, { ptr, i64, i64, i64 } } %.t465, { ptr, i64, i64, i64 } %.t467, 1
  ret { { { i64, [8 x i8] }, ptr, i64, i64 }, { ptr, i64, i64, i64 } } %.t468
if_else454:
  %.t472 = load i8, ptr %c
  %.t473 = icmp eq i8 %.t472, 123
  br i1 %.t473, label %if_then469, label %if_else470
if_then469:
  %.t474 = insertvalue { i64, [8 x i8] } undef, i64 49, 0
  %.t475 = insertvalue { { i64, [8 x i8] }, ptr, i64, i64 } undef, { i64, [8 x i8] } %.t474, 0
  %.t476 = insertvalue { { i64, [8 x i8] }, ptr, i64, i64 } %.t475, ptr getelementptr inbounds ([2 x i8], ptr @.str.15683206455558339757, i32 0, i32 0), 1
  %.t477 = load i64, ptr %line
  %.t478 = insertvalue { { i64, [8 x i8] }, ptr, i64, i64 } %.t476, i64 %.t477, 2
  %.t479 = load i64, ptr %column
  %.t480 = insertvalue { { i64, [8 x i8] }, ptr, i64, i64 } %.t478, i64 %.t479, 3
  %.t481 = insertvalue { { { i64, [8 x i8] }, ptr, i64, i64 }, { ptr, i64, i64, i64 } } undef, { { i64, [8 x i8] }, ptr, i64, i64 } %.t480, 0
  %.t482 = load { ptr, i64, i64, i64 }, ptr %l
  %.t483 = call { ptr, i64, i64, i64 } @advance({ ptr, i64, i64, i64 } %.t482)
  %.t484 = insertvalue { { { i64, [8 x i8] }, ptr, i64, i64 }, { ptr, i64, i64, i64 } } %.t481, { ptr, i64, i64, i64 } %.t483, 1
  ret { { { i64, [8 x i8] }, ptr, i64, i64 }, { ptr, i64, i64, i64 } } %.t484
if_else470:
  %.t488 = load i8, ptr %c
  %.t489 = icmp eq i8 %.t488, 125
  br i1 %.t489, label %if_then485, label %if_else486
if_then485:
  %.t490 = insertvalue { i64, [8 x i8] } undef, i64 50, 0
  %.t491 = insertvalue { { i64, [8 x i8] }, ptr, i64, i64 } undef, { i64, [8 x i8] } %.t490, 0
  %.t492 = insertvalue { { i64, [8 x i8] }, ptr, i64, i64 } %.t491, ptr getelementptr inbounds ([2 x i8], ptr @.str.17169981123852394891, i32 0, i32 0), 1
  %.t493 = load i64, ptr %line
  %.t494 = insertvalue { { i64, [8 x i8] }, ptr, i64, i64 } %.t492, i64 %.t493, 2
  %.t495 = load i64, ptr %column
  %.t496 = insertvalue { { i64, [8 x i8] }, ptr, i64, i64 } %.t494, i64 %.t495, 3
  %.t497 = insertvalue { { { i64, [8 x i8] }, ptr, i64, i64 }, { ptr, i64, i64, i64 } } undef, { { i64, [8 x i8] }, ptr, i64, i64 } %.t496, 0
  %.t498 = load { ptr, i64, i64, i64 }, ptr %l
  %.t499 = call { ptr, i64, i64, i64 } @advance({ ptr, i64, i64, i64 } %.t498)
  %.t500 = insertvalue { { { i64, [8 x i8] }, ptr, i64, i64 }, { ptr, i64, i64, i64 } } %.t497, { ptr, i64, i64, i64 } %.t499, 1
  ret { { { i64, [8 x i8] }, ptr, i64, i64 }, { ptr, i64, i64, i64 } } %.t500
if_else486:
  %.t504 = load i8, ptr %c
  %.t505 = icmp eq i8 %.t504, 59
  br i1 %.t505, label %if_then501, label %if_else502
if_then501:
  %.t506 = insertvalue { i64, [8 x i8] } undef, i64 39, 0
  %.t507 = insertvalue { { i64, [8 x i8] }, ptr, i64, i64 } undef, { i64, [8 x i8] } %.t506, 0
  %.t508 = insertvalue { { i64, [8 x i8] }, ptr, i64, i64 } %.t507, ptr getelementptr inbounds ([2 x i8], ptr @.str.14111902871104387043, i32 0, i32 0), 1
  %.t509 = load i64, ptr %line
  %.t510 = insertvalue { { i64, [8 x i8] }, ptr, i64, i64 } %.t508, i64 %.t509, 2
  %.t511 = load i64, ptr %column
  %.t512 = insertvalue { { i64, [8 x i8] }, ptr, i64, i64 } %.t510, i64 %.t511, 3
  %.t513 = insertvalue { { { i64, [8 x i8] }, ptr, i64, i64 }, { ptr, i64, i64, i64 } } undef, { { i64, [8 x i8] }, ptr, i64, i64 } %.t512, 0
  %.t514 = load { ptr, i64, i64, i64 }, ptr %l
  %.t515 = call { ptr, i64, i64, i64 } @advance({ ptr, i64, i64, i64 } %.t514)
  %.t516 = insertvalue { { { i64, [8 x i8] }, ptr, i64, i64 }, { ptr, i64, i64, i64 } } %.t513, { ptr, i64, i64, i64 } %.t515, 1
  ret { { { i64, [8 x i8] }, ptr, i64, i64 }, { ptr, i64, i64, i64 } } %.t516
if_else502:
  %.t520 = load i8, ptr %c
  %.t521 = icmp eq i8 %.t520, 44
  br i1 %.t521, label %if_then517, label %if_else518
if_then517:
  %.t522 = insertvalue { i64, [8 x i8] } undef, i64 40, 0
  %.t523 = insertvalue { { i64, [8 x i8] }, ptr, i64, i64 } undef, { i64, [8 x i8] } %.t522, 0
  %.t524 = insertvalue { { i64, [8 x i8] }, ptr, i64, i64 } %.t523, ptr getelementptr inbounds ([2 x i8], ptr @.str.11693505714082325371, i32 0, i32 0), 1
  %.t525 = load i64, ptr %line
  %.t526 = insertvalue { { i64, [8 x i8] }, ptr, i64, i64 } %.t524, i64 %.t525, 2
  %.t527 = load i64, ptr %column
  %.t528 = insertvalue { { i64, [8 x i8] }, ptr, i64, i64 } %.t526, i64 %.t527, 3
  %.t529 = insertvalue { { { i64, [8 x i8] }, ptr, i64, i64 }, { ptr, i64, i64, i64 } } undef, { { i64, [8 x i8] }, ptr, i64, i64 } %.t528, 0
  %.t530 = load { ptr, i64, i64, i64 }, ptr %l
  %.t531 = call { ptr, i64, i64, i64 } @advance({ ptr, i64, i64, i64 } %.t530)
  %.t532 = insertvalue { { { i64, [8 x i8] }, ptr, i64, i64 }, { ptr, i64, i64, i64 } } %.t529, { ptr, i64, i64, i64 } %.t531, 1
  ret { { { i64, [8 x i8] }, ptr, i64, i64 }, { ptr, i64, i64, i64 } } %.t532
if_else518:
  %.t536 = load i8, ptr %c
  %.t537 = icmp eq i8 %.t536, 46
  br i1 %.t537, label %if_then533, label %if_else534
if_then533:
  %.t538 = insertvalue { i64, [8 x i8] } undef, i64 41, 0
  %.t539 = insertvalue { { i64, [8 x i8] }, ptr, i64, i64 } undef, { i64, [8 x i8] } %.t538, 0
  %.t540 = insertvalue { { i64, [8 x i8] }, ptr, i64, i64 } %.t539, ptr getelementptr inbounds ([2 x i8], ptr @.str.2249644112086988589, i32 0, i32 0), 1
  %.t541 = load i64, ptr %line
  %.t542 = insertvalue { { i64, [8 x i8] }, ptr, i64, i64 } %.t540, i64 %.t541, 2
  %.t543 = load i64, ptr %column
  %.t544 = insertvalue { { i64, [8 x i8] }, ptr, i64, i64 } %.t542, i64 %.t543, 3
  %.t545 = insertvalue { { { i64, [8 x i8] }, ptr, i64, i64 }, { ptr, i64, i64, i64 } } undef, { { i64, [8 x i8] }, ptr, i64, i64 } %.t544, 0
  %.t546 = load { ptr, i64, i64, i64 }, ptr %l
  %.t547 = call { ptr, i64, i64, i64 } @advance({ ptr, i64, i64, i64 } %.t546)
  %.t548 = insertvalue { { { i64, [8 x i8] }, ptr, i64, i64 }, { ptr, i64, i64, i64 } } %.t545, { ptr, i64, i64, i64 } %.t547, 1
  ret { { { i64, [8 x i8] }, ptr, i64, i64 }, { ptr, i64, i64, i64 } } %.t548
if_else534:
  %.t552 = load i8, ptr %c
  %.t553 = icmp eq i8 %.t552, 58
  br i1 %.t553, label %if_then549, label %if_else550
if_then549:
  %.t554 = insertvalue { i64, [8 x i8] } undef, i64 38, 0
  %.t555 = insertvalue { { i64, [8 x i8] }, ptr, i64, i64 } undef, { i64, [8 x i8] } %.t554, 0
  %.t556 = insertvalue { { i64, [8 x i8] }, ptr, i64, i64 } %.t555, ptr getelementptr inbounds ([2 x i8], ptr @.str.1652154084197249351, i32 0, i32 0), 1
  %.t557 = load i64, ptr %line
  %.t558 = insertvalue { { i64, [8 x i8] }, ptr, i64, i64 } %.t556, i64 %.t557, 2
  %.t559 = load i64, ptr %column
  %.t560 = insertvalue { { i64, [8 x i8] }, ptr, i64, i64 } %.t558, i64 %.t559, 3
  %.t561 = insertvalue { { { i64, [8 x i8] }, ptr, i64, i64 }, { ptr, i64, i64, i64 } } undef, { { i64, [8 x i8] }, ptr, i64, i64 } %.t560, 0
  %.t562 = load { ptr, i64, i64, i64 }, ptr %l
  %.t563 = call { ptr, i64, i64, i64 } @advance({ ptr, i64, i64, i64 } %.t562)
  %.t564 = insertvalue { { { i64, [8 x i8] }, ptr, i64, i64 }, { ptr, i64, i64, i64 } } %.t561, { ptr, i64, i64, i64 } %.t563, 1
  ret { { { i64, [8 x i8] }, ptr, i64, i64 }, { ptr, i64, i64, i64 } } %.t564
if_else550:
  %.t565 = insertvalue { i64, [8 x i8] } undef, i64 52, 0
  %.t566 = insertvalue { { i64, [8 x i8] }, ptr, i64, i64 } undef, { i64, [8 x i8] } %.t565, 0
  %.t567 = insertvalue { { i64, [8 x i8] }, ptr, i64, i64 } %.t566, ptr getelementptr inbounds ([1 x i8], ptr @.str.18151106360299273605, i32 0, i32 0), 1
  %.t568 = load i64, ptr %line
  %.t569 = insertvalue { { i64, [8 x i8] }, ptr, i64, i64 } %.t567, i64 %.t568, 2
  %.t570 = load i64, ptr %column
  %.t571 = insertvalue { { i64, [8 x i8] }, ptr, i64, i64 } %.t569, i64 %.t570, 3
  %.t572 = insertvalue { { { i64, [8 x i8] }, ptr, i64, i64 }, { ptr, i64, i64, i64 } } undef, { { i64, [8 x i8] }, ptr, i64, i64 } %.t571, 0
  %.t573 = load { ptr, i64, i64, i64 }, ptr %l
  %.t574 = call { ptr, i64, i64, i64 } @advance({ ptr, i64, i64, i64 } %.t573)
  %.t575 = insertvalue { { { i64, [8 x i8] }, ptr, i64, i64 }, { ptr, i64, i64, i64 } } %.t572, { ptr, i64, i64, i64 } %.t574, 1
  ret { { { i64, [8 x i8] }, ptr, i64, i64 }, { ptr, i64, i64, i64 } } %.t575
}


@.str.18151106360299273605 = private unnamed_addr constant [1 x i8] c"\00"
@.str.15882864822835482160 = private unnamed_addr constant [2 x i8] c"(\00"
@.str.2200420175901286535 = private unnamed_addr constant [2 x i8] c")\00"
@.str.16740139525474306003 = private unnamed_addr constant [2 x i8] c"[\00"
@.str.810667871647353583 = private unnamed_addr constant [2 x i8] c"]\00"
@.str.15683206455558339757 = private unnamed_addr constant [2 x i8] c"{\00"
@.str.17169981123852394891 = private unnamed_addr constant [2 x i8] c"}\00"
@.str.14111902871104387043 = private unnamed_addr constant [2 x i8] c";\00"
@.str.11693505714082325371 = private unnamed_addr constant [2 x i8] c",\00"
@.str.2249644112086988589 = private unnamed_addr constant [2 x i8] c".\00"
@.str.1652154084197249351 = private unnamed_addr constant [2 x i8] c":\00"
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
  switch i64 %.t600, label %match_unreachable604 [
    i64 0, label %match_arm602
    i64 1, label %match_arm603
  ]
match_arm602:
  %.t605 = alloca { i64, [8 x i8] }
  store { i64, [8 x i8] } %.t599, ptr %.t605
  %.t606 = getelementptr inbounds { i64, [8 x i8] }, ptr %.t605, i32 0, i32 1
  %.t607 = getelementptr inbounds i8, ptr %.t606, i32 0
  %.t608 = alloca i64
  %.t609 = load i64, ptr %.t607
  store i64 %.t609, ptr %.t608
  br label %match_merge601
match_arm603:
  %.t610 = alloca { i64, [8 x i8] }
  store { i64, [8 x i8] } %.t599, ptr %.t610
  %.t611 = getelementptr inbounds { i64, [8 x i8] }, ptr %.t610, i32 0, i32 1
  %.t612 = getelementptr inbounds i8, ptr %.t611, i32 0
  %.t613 = alloca { i64, [8 x i8] }
  %.t614 = load { i64, [8 x i8] }, ptr %.t612
  store { i64, [8 x i8] } %.t614, ptr %.t613
  br label %match_merge601
match_unreachable604:
  unreachable
match_merge601:
  %.t615 = phi i1 [ 1, %match_arm602 ], [ 0, %match_arm603 ]
  ret i1 %.t615
}

define i1 @Result$$i64$Error$$__is_err({ i64, [8 x i8] } %res) {
entry:
  %res.addr = alloca { i64, [8 x i8] }
  store { i64, [8 x i8] } %res, ptr %res.addr
  %.t616 = load { i64, [8 x i8] }, ptr %res.addr
  %.t617 = extractvalue { i64, [8 x i8] } %.t616, 0
  switch i64 %.t617, label %match_unreachable621 [
    i64 0, label %match_arm619
    i64 1, label %match_arm620
  ]
match_arm619:
  %.t622 = alloca { i64, [8 x i8] }
  store { i64, [8 x i8] } %.t616, ptr %.t622
  %.t623 = getelementptr inbounds { i64, [8 x i8] }, ptr %.t622, i32 0, i32 1
  %.t624 = getelementptr inbounds i8, ptr %.t623, i32 0
  %.t625 = alloca i64
  %.t626 = load i64, ptr %.t624
  store i64 %.t626, ptr %.t625
  br label %match_merge618
match_arm620:
  %.t627 = alloca { i64, [8 x i8] }
  store { i64, [8 x i8] } %.t616, ptr %.t627
  %.t628 = getelementptr inbounds { i64, [8 x i8] }, ptr %.t627, i32 0, i32 1
  %.t629 = getelementptr inbounds i8, ptr %.t628, i32 0
  %.t630 = alloca { i64, [8 x i8] }
  %.t631 = load { i64, [8 x i8] }, ptr %.t629
  store { i64, [8 x i8] } %.t631, ptr %.t630
  br label %match_merge618
match_unreachable621:
  unreachable
match_merge618:
  %.t632 = phi i1 [ 0, %match_arm619 ], [ 1, %match_arm620 ]
  ret i1 %.t632
}

define i64 @Result$$i64$Error$$__unwrap_or({ i64, [8 x i8] } %res, i64 %fallback) {
entry:
  %res.addr = alloca { i64, [8 x i8] }
  store { i64, [8 x i8] } %res, ptr %res.addr
  %fallback.addr = alloca i64
  store i64 %fallback, ptr %fallback.addr
  %.t633 = load { i64, [8 x i8] }, ptr %res.addr
  %.t634 = extractvalue { i64, [8 x i8] } %.t633, 0
  switch i64 %.t634, label %match_unreachable638 [
    i64 0, label %match_arm636
    i64 1, label %match_arm637
  ]
match_arm636:
  %.t639 = alloca { i64, [8 x i8] }
  store { i64, [8 x i8] } %.t633, ptr %.t639
  %.t640 = getelementptr inbounds { i64, [8 x i8] }, ptr %.t639, i32 0, i32 1
  %.t641 = getelementptr inbounds i8, ptr %.t640, i32 0
  %.t642 = alloca i64
  %.t643 = load i64, ptr %.t641
  store i64 %.t643, ptr %.t642
  %.t644 = load i64, ptr %.t642
  %.t645 = trunc i64 %.t644 to i32
  br label %match_merge635
match_arm637:
  %.t646 = alloca { i64, [8 x i8] }
  store { i64, [8 x i8] } %.t633, ptr %.t646
  %.t647 = getelementptr inbounds { i64, [8 x i8] }, ptr %.t646, i32 0, i32 1
  %.t648 = getelementptr inbounds i8, ptr %.t647, i32 0
  %.t649 = alloca { i64, [8 x i8] }
  %.t650 = load { i64, [8 x i8] }, ptr %.t648
  store { i64, [8 x i8] } %.t650, ptr %.t649
  %.t651 = load i64, ptr %fallback.addr
  %.t652 = trunc i64 %.t651 to i32
  br label %match_merge635
match_unreachable638:
  unreachable
match_merge635:
  %.t653 = phi i32 [ %.t645, %match_arm636 ], [ %.t652, %match_arm637 ]
  %.t654 = sext i32 %.t653 to i64
  ret i64 %.t654
}

define i1 @Result$$u64$Error$$__is_ok({ i64, [8 x i8] } %res) {
entry:
  %res.addr = alloca { i64, [8 x i8] }
  store { i64, [8 x i8] } %res, ptr %res.addr
  %.t655 = load { i64, [8 x i8] }, ptr %res.addr
  %.t656 = extractvalue { i64, [8 x i8] } %.t655, 0
  switch i64 %.t656, label %match_unreachable660 [
    i64 0, label %match_arm658
    i64 1, label %match_arm659
  ]
match_arm658:
  %.t661 = alloca { i64, [8 x i8] }
  store { i64, [8 x i8] } %.t655, ptr %.t661
  %.t662 = getelementptr inbounds { i64, [8 x i8] }, ptr %.t661, i32 0, i32 1
  %.t663 = getelementptr inbounds i8, ptr %.t662, i32 0
  %.t664 = alloca i64
  %.t665 = load i64, ptr %.t663
  store i64 %.t665, ptr %.t664
  br label %match_merge657
match_arm659:
  %.t666 = alloca { i64, [8 x i8] }
  store { i64, [8 x i8] } %.t655, ptr %.t666
  %.t667 = getelementptr inbounds { i64, [8 x i8] }, ptr %.t666, i32 0, i32 1
  %.t668 = getelementptr inbounds i8, ptr %.t667, i32 0
  %.t669 = alloca { i64, [8 x i8] }
  %.t670 = load { i64, [8 x i8] }, ptr %.t668
  store { i64, [8 x i8] } %.t670, ptr %.t669
  br label %match_merge657
match_unreachable660:
  unreachable
match_merge657:
  %.t671 = phi i1 [ 1, %match_arm658 ], [ 0, %match_arm659 ]
  ret i1 %.t671
}

define i1 @Result$$u64$Error$$__is_err({ i64, [8 x i8] } %res) {
entry:
  %res.addr = alloca { i64, [8 x i8] }
  store { i64, [8 x i8] } %res, ptr %res.addr
  %.t672 = load { i64, [8 x i8] }, ptr %res.addr
  %.t673 = extractvalue { i64, [8 x i8] } %.t672, 0
  switch i64 %.t673, label %match_unreachable677 [
    i64 0, label %match_arm675
    i64 1, label %match_arm676
  ]
match_arm675:
  %.t678 = alloca { i64, [8 x i8] }
  store { i64, [8 x i8] } %.t672, ptr %.t678
  %.t679 = getelementptr inbounds { i64, [8 x i8] }, ptr %.t678, i32 0, i32 1
  %.t680 = getelementptr inbounds i8, ptr %.t679, i32 0
  %.t681 = alloca i64
  %.t682 = load i64, ptr %.t680
  store i64 %.t682, ptr %.t681
  br label %match_merge674
match_arm676:
  %.t683 = alloca { i64, [8 x i8] }
  store { i64, [8 x i8] } %.t672, ptr %.t683
  %.t684 = getelementptr inbounds { i64, [8 x i8] }, ptr %.t683, i32 0, i32 1
  %.t685 = getelementptr inbounds i8, ptr %.t684, i32 0
  %.t686 = alloca { i64, [8 x i8] }
  %.t687 = load { i64, [8 x i8] }, ptr %.t685
  store { i64, [8 x i8] } %.t687, ptr %.t686
  br label %match_merge674
match_unreachable677:
  unreachable
match_merge674:
  %.t688 = phi i1 [ 0, %match_arm675 ], [ 1, %match_arm676 ]
  ret i1 %.t688
}

define i64 @Result$$u64$Error$$__unwrap_or({ i64, [8 x i8] } %res, i64 %fallback) {
entry:
  %res.addr = alloca { i64, [8 x i8] }
  store { i64, [8 x i8] } %res, ptr %res.addr
  %fallback.addr = alloca i64
  store i64 %fallback, ptr %fallback.addr
  %.t689 = load { i64, [8 x i8] }, ptr %res.addr
  %.t690 = extractvalue { i64, [8 x i8] } %.t689, 0
  switch i64 %.t690, label %match_unreachable694 [
    i64 0, label %match_arm692
    i64 1, label %match_arm693
  ]
match_arm692:
  %.t695 = alloca { i64, [8 x i8] }
  store { i64, [8 x i8] } %.t689, ptr %.t695
  %.t696 = getelementptr inbounds { i64, [8 x i8] }, ptr %.t695, i32 0, i32 1
  %.t697 = getelementptr inbounds i8, ptr %.t696, i32 0
  %.t698 = alloca i64
  %.t699 = load i64, ptr %.t697
  store i64 %.t699, ptr %.t698
  %.t700 = load i64, ptr %.t698
  %.t701 = trunc i64 %.t700 to i32
  br label %match_merge691
match_arm693:
  %.t702 = alloca { i64, [8 x i8] }
  store { i64, [8 x i8] } %.t689, ptr %.t702
  %.t703 = getelementptr inbounds { i64, [8 x i8] }, ptr %.t702, i32 0, i32 1
  %.t704 = getelementptr inbounds i8, ptr %.t703, i32 0
  %.t705 = alloca { i64, [8 x i8] }
  %.t706 = load { i64, [8 x i8] }, ptr %.t704
  store { i64, [8 x i8] } %.t706, ptr %.t705
  %.t707 = load i64, ptr %fallback.addr
  %.t708 = trunc i64 %.t707 to i32
  br label %match_merge691
match_unreachable694:
  unreachable
match_merge691:
  %.t709 = phi i32 [ %.t701, %match_arm692 ], [ %.t708, %match_arm693 ]
  %.t710 = sext i32 %.t709 to i64
  ret i64 %.t710
}

define i1 @Result$$str$Error$$__is_ok({ i64, [8 x i8] } %res) {
entry:
  %res.addr = alloca { i64, [8 x i8] }
  store { i64, [8 x i8] } %res, ptr %res.addr
  %.t711 = load { i64, [8 x i8] }, ptr %res.addr
  %.t712 = extractvalue { i64, [8 x i8] } %.t711, 0
  switch i64 %.t712, label %match_unreachable716 [
    i64 0, label %match_arm714
    i64 1, label %match_arm715
  ]
match_arm714:
  %.t717 = alloca { i64, [8 x i8] }
  store { i64, [8 x i8] } %.t711, ptr %.t717
  %.t718 = getelementptr inbounds { i64, [8 x i8] }, ptr %.t717, i32 0, i32 1
  %.t719 = getelementptr inbounds i8, ptr %.t718, i32 0
  %.t720 = alloca ptr
  %.t721 = load ptr, ptr %.t719
  store ptr %.t721, ptr %.t720
  br label %match_merge713
match_arm715:
  %.t722 = alloca { i64, [8 x i8] }
  store { i64, [8 x i8] } %.t711, ptr %.t722
  %.t723 = getelementptr inbounds { i64, [8 x i8] }, ptr %.t722, i32 0, i32 1
  %.t724 = getelementptr inbounds i8, ptr %.t723, i32 0
  %.t725 = alloca { i64, [8 x i8] }
  %.t726 = load { i64, [8 x i8] }, ptr %.t724
  store { i64, [8 x i8] } %.t726, ptr %.t725
  br label %match_merge713
match_unreachable716:
  unreachable
match_merge713:
  %.t727 = phi i1 [ 1, %match_arm714 ], [ 0, %match_arm715 ]
  ret i1 %.t727
}

define i1 @Result$$str$Error$$__is_err({ i64, [8 x i8] } %res) {
entry:
  %res.addr = alloca { i64, [8 x i8] }
  store { i64, [8 x i8] } %res, ptr %res.addr
  %.t728 = load { i64, [8 x i8] }, ptr %res.addr
  %.t729 = extractvalue { i64, [8 x i8] } %.t728, 0
  switch i64 %.t729, label %match_unreachable733 [
    i64 0, label %match_arm731
    i64 1, label %match_arm732
  ]
match_arm731:
  %.t734 = alloca { i64, [8 x i8] }
  store { i64, [8 x i8] } %.t728, ptr %.t734
  %.t735 = getelementptr inbounds { i64, [8 x i8] }, ptr %.t734, i32 0, i32 1
  %.t736 = getelementptr inbounds i8, ptr %.t735, i32 0
  %.t737 = alloca ptr
  %.t738 = load ptr, ptr %.t736
  store ptr %.t738, ptr %.t737
  br label %match_merge730
match_arm732:
  %.t739 = alloca { i64, [8 x i8] }
  store { i64, [8 x i8] } %.t728, ptr %.t739
  %.t740 = getelementptr inbounds { i64, [8 x i8] }, ptr %.t739, i32 0, i32 1
  %.t741 = getelementptr inbounds i8, ptr %.t740, i32 0
  %.t742 = alloca { i64, [8 x i8] }
  %.t743 = load { i64, [8 x i8] }, ptr %.t741
  store { i64, [8 x i8] } %.t743, ptr %.t742
  br label %match_merge730
match_unreachable733:
  unreachable
match_merge730:
  %.t744 = phi i1 [ 0, %match_arm731 ], [ 1, %match_arm732 ]
  ret i1 %.t744
}

define ptr @Result$$str$Error$$__unwrap_or({ i64, [8 x i8] } %res, ptr %fallback) {
entry:
  %res.addr = alloca { i64, [8 x i8] }
  store { i64, [8 x i8] } %res, ptr %res.addr
  %fallback.addr = alloca ptr
  store ptr %fallback, ptr %fallback.addr
  %.t745 = load { i64, [8 x i8] }, ptr %res.addr
  %.t746 = extractvalue { i64, [8 x i8] } %.t745, 0
  switch i64 %.t746, label %match_unreachable750 [
    i64 0, label %match_arm748
    i64 1, label %match_arm749
  ]
match_arm748:
  %.t751 = alloca { i64, [8 x i8] }
  store { i64, [8 x i8] } %.t745, ptr %.t751
  %.t752 = getelementptr inbounds { i64, [8 x i8] }, ptr %.t751, i32 0, i32 1
  %.t753 = getelementptr inbounds i8, ptr %.t752, i32 0
  %.t754 = alloca ptr
  %.t755 = load ptr, ptr %.t753
  store ptr %.t755, ptr %.t754
  %.t756 = load ptr, ptr %.t754
  %.t757 = ptrtoint ptr %.t756 to i32
  br label %match_merge747
match_arm749:
  %.t758 = alloca { i64, [8 x i8] }
  store { i64, [8 x i8] } %.t745, ptr %.t758
  %.t759 = getelementptr inbounds { i64, [8 x i8] }, ptr %.t758, i32 0, i32 1
  %.t760 = getelementptr inbounds i8, ptr %.t759, i32 0
  %.t761 = alloca { i64, [8 x i8] }
  %.t762 = load { i64, [8 x i8] }, ptr %.t760
  store { i64, [8 x i8] } %.t762, ptr %.t761
  %.t763 = load ptr, ptr %fallback.addr
  %.t764 = ptrtoint ptr %.t763 to i32
  br label %match_merge747
match_unreachable750:
  unreachable
match_merge747:
  %.t765 = phi i32 [ %.t757, %match_arm748 ], [ %.t764, %match_arm749 ]
  %.t766 = inttoptr i32 %.t765 to ptr
  ret ptr %.t766
}

define i1 @Result$$ptr$Error$$__is_ok({ i64, [8 x i8] } %res) {
entry:
  %res.addr = alloca { i64, [8 x i8] }
  store { i64, [8 x i8] } %res, ptr %res.addr
  %.t767 = load { i64, [8 x i8] }, ptr %res.addr
  %.t768 = extractvalue { i64, [8 x i8] } %.t767, 0
  switch i64 %.t768, label %match_unreachable772 [
    i64 0, label %match_arm770
    i64 1, label %match_arm771
  ]
match_arm770:
  %.t773 = alloca { i64, [8 x i8] }
  store { i64, [8 x i8] } %.t767, ptr %.t773
  %.t774 = getelementptr inbounds { i64, [8 x i8] }, ptr %.t773, i32 0, i32 1
  %.t775 = getelementptr inbounds i8, ptr %.t774, i32 0
  %.t776 = alloca ptr
  %.t777 = load ptr, ptr %.t775
  store ptr %.t777, ptr %.t776
  br label %match_merge769
match_arm771:
  %.t778 = alloca { i64, [8 x i8] }
  store { i64, [8 x i8] } %.t767, ptr %.t778
  %.t779 = getelementptr inbounds { i64, [8 x i8] }, ptr %.t778, i32 0, i32 1
  %.t780 = getelementptr inbounds i8, ptr %.t779, i32 0
  %.t781 = alloca { i64, [8 x i8] }
  %.t782 = load { i64, [8 x i8] }, ptr %.t780
  store { i64, [8 x i8] } %.t782, ptr %.t781
  br label %match_merge769
match_unreachable772:
  unreachable
match_merge769:
  %.t783 = phi i1 [ 1, %match_arm770 ], [ 0, %match_arm771 ]
  ret i1 %.t783
}

define i1 @Result$$ptr$Error$$__is_err({ i64, [8 x i8] } %res) {
entry:
  %res.addr = alloca { i64, [8 x i8] }
  store { i64, [8 x i8] } %res, ptr %res.addr
  %.t784 = load { i64, [8 x i8] }, ptr %res.addr
  %.t785 = extractvalue { i64, [8 x i8] } %.t784, 0
  switch i64 %.t785, label %match_unreachable789 [
    i64 0, label %match_arm787
    i64 1, label %match_arm788
  ]
match_arm787:
  %.t790 = alloca { i64, [8 x i8] }
  store { i64, [8 x i8] } %.t784, ptr %.t790
  %.t791 = getelementptr inbounds { i64, [8 x i8] }, ptr %.t790, i32 0, i32 1
  %.t792 = getelementptr inbounds i8, ptr %.t791, i32 0
  %.t793 = alloca ptr
  %.t794 = load ptr, ptr %.t792
  store ptr %.t794, ptr %.t793
  br label %match_merge786
match_arm788:
  %.t795 = alloca { i64, [8 x i8] }
  store { i64, [8 x i8] } %.t784, ptr %.t795
  %.t796 = getelementptr inbounds { i64, [8 x i8] }, ptr %.t795, i32 0, i32 1
  %.t797 = getelementptr inbounds i8, ptr %.t796, i32 0
  %.t798 = alloca { i64, [8 x i8] }
  %.t799 = load { i64, [8 x i8] }, ptr %.t797
  store { i64, [8 x i8] } %.t799, ptr %.t798
  br label %match_merge786
match_unreachable789:
  unreachable
match_merge786:
  %.t800 = phi i1 [ 0, %match_arm787 ], [ 1, %match_arm788 ]
  ret i1 %.t800
}

define ptr @Result$$ptr$Error$$__unwrap_or({ i64, [8 x i8] } %res, ptr %fallback) {
entry:
  %res.addr = alloca { i64, [8 x i8] }
  store { i64, [8 x i8] } %res, ptr %res.addr
  %fallback.addr = alloca ptr
  store ptr %fallback, ptr %fallback.addr
  %.t801 = load { i64, [8 x i8] }, ptr %res.addr
  %.t802 = extractvalue { i64, [8 x i8] } %.t801, 0
  switch i64 %.t802, label %match_unreachable806 [
    i64 0, label %match_arm804
    i64 1, label %match_arm805
  ]
match_arm804:
  %.t807 = alloca { i64, [8 x i8] }
  store { i64, [8 x i8] } %.t801, ptr %.t807
  %.t808 = getelementptr inbounds { i64, [8 x i8] }, ptr %.t807, i32 0, i32 1
  %.t809 = getelementptr inbounds i8, ptr %.t808, i32 0
  %.t810 = alloca ptr
  %.t811 = load ptr, ptr %.t809
  store ptr %.t811, ptr %.t810
  %.t812 = load ptr, ptr %.t810
  %.t813 = ptrtoint ptr %.t812 to i32
  br label %match_merge803
match_arm805:
  %.t814 = alloca { i64, [8 x i8] }
  store { i64, [8 x i8] } %.t801, ptr %.t814
  %.t815 = getelementptr inbounds { i64, [8 x i8] }, ptr %.t814, i32 0, i32 1
  %.t816 = getelementptr inbounds i8, ptr %.t815, i32 0
  %.t817 = alloca { i64, [8 x i8] }
  %.t818 = load { i64, [8 x i8] }, ptr %.t816
  store { i64, [8 x i8] } %.t818, ptr %.t817
  %.t819 = load ptr, ptr %fallback.addr
  %.t820 = ptrtoint ptr %.t819 to i32
  br label %match_merge803
match_unreachable806:
  unreachable
match_merge803:
  %.t821 = phi i32 [ %.t813, %match_arm804 ], [ %.t820, %match_arm805 ]
  %.t822 = inttoptr i32 %.t821 to ptr
  ret ptr %.t822
}

define i1 @Result$$bool$Error$$__is_ok({ i64, [8 x i8] } %res) {
entry:
  %res.addr = alloca { i64, [8 x i8] }
  store { i64, [8 x i8] } %res, ptr %res.addr
  %.t823 = load { i64, [8 x i8] }, ptr %res.addr
  %.t824 = extractvalue { i64, [8 x i8] } %.t823, 0
  switch i64 %.t824, label %match_unreachable828 [
    i64 0, label %match_arm826
    i64 1, label %match_arm827
  ]
match_arm826:
  %.t829 = alloca { i64, [8 x i8] }
  store { i64, [8 x i8] } %.t823, ptr %.t829
  %.t830 = getelementptr inbounds { i64, [8 x i8] }, ptr %.t829, i32 0, i32 1
  %.t831 = getelementptr inbounds i8, ptr %.t830, i32 0
  %.t832 = alloca i1
  %.t833 = load i1, ptr %.t831
  store i1 %.t833, ptr %.t832
  br label %match_merge825
match_arm827:
  %.t834 = alloca { i64, [8 x i8] }
  store { i64, [8 x i8] } %.t823, ptr %.t834
  %.t835 = getelementptr inbounds { i64, [8 x i8] }, ptr %.t834, i32 0, i32 1
  %.t836 = getelementptr inbounds i8, ptr %.t835, i32 0
  %.t837 = alloca { i64, [8 x i8] }
  %.t838 = load { i64, [8 x i8] }, ptr %.t836
  store { i64, [8 x i8] } %.t838, ptr %.t837
  br label %match_merge825
match_unreachable828:
  unreachable
match_merge825:
  %.t839 = phi i1 [ 1, %match_arm826 ], [ 0, %match_arm827 ]
  ret i1 %.t839
}

define i1 @Result$$bool$Error$$__is_err({ i64, [8 x i8] } %res) {
entry:
  %res.addr = alloca { i64, [8 x i8] }
  store { i64, [8 x i8] } %res, ptr %res.addr
  %.t840 = load { i64, [8 x i8] }, ptr %res.addr
  %.t841 = extractvalue { i64, [8 x i8] } %.t840, 0
  switch i64 %.t841, label %match_unreachable845 [
    i64 0, label %match_arm843
    i64 1, label %match_arm844
  ]
match_arm843:
  %.t846 = alloca { i64, [8 x i8] }
  store { i64, [8 x i8] } %.t840, ptr %.t846
  %.t847 = getelementptr inbounds { i64, [8 x i8] }, ptr %.t846, i32 0, i32 1
  %.t848 = getelementptr inbounds i8, ptr %.t847, i32 0
  %.t849 = alloca i1
  %.t850 = load i1, ptr %.t848
  store i1 %.t850, ptr %.t849
  br label %match_merge842
match_arm844:
  %.t851 = alloca { i64, [8 x i8] }
  store { i64, [8 x i8] } %.t840, ptr %.t851
  %.t852 = getelementptr inbounds { i64, [8 x i8] }, ptr %.t851, i32 0, i32 1
  %.t853 = getelementptr inbounds i8, ptr %.t852, i32 0
  %.t854 = alloca { i64, [8 x i8] }
  %.t855 = load { i64, [8 x i8] }, ptr %.t853
  store { i64, [8 x i8] } %.t855, ptr %.t854
  br label %match_merge842
match_unreachable845:
  unreachable
match_merge842:
  %.t856 = phi i1 [ 0, %match_arm843 ], [ 1, %match_arm844 ]
  ret i1 %.t856
}

define i1 @Result$$bool$Error$$__unwrap_or({ i64, [8 x i8] } %res, i1 %fallback) {
entry:
  %res.addr = alloca { i64, [8 x i8] }
  store { i64, [8 x i8] } %res, ptr %res.addr
  %fallback.addr = alloca i1
  store i1 %fallback, ptr %fallback.addr
  %.t857 = load { i64, [8 x i8] }, ptr %res.addr
  %.t858 = extractvalue { i64, [8 x i8] } %.t857, 0
  switch i64 %.t858, label %match_unreachable862 [
    i64 0, label %match_arm860
    i64 1, label %match_arm861
  ]
match_arm860:
  %.t863 = alloca { i64, [8 x i8] }
  store { i64, [8 x i8] } %.t857, ptr %.t863
  %.t864 = getelementptr inbounds { i64, [8 x i8] }, ptr %.t863, i32 0, i32 1
  %.t865 = getelementptr inbounds i8, ptr %.t864, i32 0
  %.t866 = alloca i1
  %.t867 = load i1, ptr %.t865
  store i1 %.t867, ptr %.t866
  %.t868 = load i1, ptr %.t866
  %.t869 = zext i1 %.t868 to i32
  br label %match_merge859
match_arm861:
  %.t870 = alloca { i64, [8 x i8] }
  store { i64, [8 x i8] } %.t857, ptr %.t870
  %.t871 = getelementptr inbounds { i64, [8 x i8] }, ptr %.t870, i32 0, i32 1
  %.t872 = getelementptr inbounds i8, ptr %.t871, i32 0
  %.t873 = alloca { i64, [8 x i8] }
  %.t874 = load { i64, [8 x i8] }, ptr %.t872
  store { i64, [8 x i8] } %.t874, ptr %.t873
  %.t875 = load i1, ptr %fallback.addr
  %.t876 = zext i1 %.t875 to i32
  br label %match_merge859
match_unreachable862:
  unreachable
match_merge859:
  %.t877 = phi i32 [ %.t869, %match_arm860 ], [ %.t876, %match_arm861 ]
  %.t878 = trunc i32 %.t877 to i1
  ret i1 %.t878
}

define i1 @Option$$i64$$__is_some({ i64, [8 x i8] } %opt) {
entry:
  %opt.addr = alloca { i64, [8 x i8] }
  store { i64, [8 x i8] } %opt, ptr %opt.addr
  %.t879 = load { i64, [8 x i8] }, ptr %opt.addr
  %.t880 = extractvalue { i64, [8 x i8] } %.t879, 0
  switch i64 %.t880, label %match_arm883 [
    i64 0, label %match_arm882
  ]
match_arm882:
  %.t884 = alloca { i64, [8 x i8] }
  store { i64, [8 x i8] } %.t879, ptr %.t884
  %.t885 = getelementptr inbounds { i64, [8 x i8] }, ptr %.t884, i32 0, i32 1
  %.t886 = getelementptr inbounds i8, ptr %.t885, i32 0
  %.t887 = alloca i64
  %.t888 = load i64, ptr %.t886
  store i64 %.t888, ptr %.t887
  br label %match_merge881
match_arm883:
  br label %match_merge881
match_merge881:
  %.t889 = phi i1 [ 1, %match_arm882 ], [ 0, %match_arm883 ]
  ret i1 %.t889
}

define i1 @Option$$i64$$__is_none({ i64, [8 x i8] } %opt) {
entry:
  %opt.addr = alloca { i64, [8 x i8] }
  store { i64, [8 x i8] } %opt, ptr %opt.addr
  %.t890 = load { i64, [8 x i8] }, ptr %opt.addr
  %.t891 = extractvalue { i64, [8 x i8] } %.t890, 0
  switch i64 %.t891, label %match_arm894 [
    i64 0, label %match_arm893
  ]
match_arm893:
  %.t895 = alloca { i64, [8 x i8] }
  store { i64, [8 x i8] } %.t890, ptr %.t895
  %.t896 = getelementptr inbounds { i64, [8 x i8] }, ptr %.t895, i32 0, i32 1
  %.t897 = getelementptr inbounds i8, ptr %.t896, i32 0
  %.t898 = alloca i64
  %.t899 = load i64, ptr %.t897
  store i64 %.t899, ptr %.t898
  br label %match_merge892
match_arm894:
  br label %match_merge892
match_merge892:
  %.t900 = phi i1 [ 0, %match_arm893 ], [ 1, %match_arm894 ]
  ret i1 %.t900
}

define i64 @Option$$i64$$__unwrap_or({ i64, [8 x i8] } %opt, i64 %fallback) {
entry:
  %opt.addr = alloca { i64, [8 x i8] }
  store { i64, [8 x i8] } %opt, ptr %opt.addr
  %fallback.addr = alloca i64
  store i64 %fallback, ptr %fallback.addr
  %.t901 = load { i64, [8 x i8] }, ptr %opt.addr
  %.t902 = extractvalue { i64, [8 x i8] } %.t901, 0
  switch i64 %.t902, label %match_arm905 [
    i64 0, label %match_arm904
  ]
match_arm904:
  %.t906 = alloca { i64, [8 x i8] }
  store { i64, [8 x i8] } %.t901, ptr %.t906
  %.t907 = getelementptr inbounds { i64, [8 x i8] }, ptr %.t906, i32 0, i32 1
  %.t908 = getelementptr inbounds i8, ptr %.t907, i32 0
  %.t909 = alloca i64
  %.t910 = load i64, ptr %.t908
  store i64 %.t910, ptr %.t909
  %.t911 = load i64, ptr %.t909
  %.t912 = trunc i64 %.t911 to i32
  br label %match_merge903
match_arm905:
  %.t913 = load i64, ptr %fallback.addr
  %.t914 = trunc i64 %.t913 to i32
  br label %match_merge903
match_merge903:
  %.t915 = phi i32 [ %.t912, %match_arm904 ], [ %.t914, %match_arm905 ]
  %.t916 = sext i32 %.t915 to i64
  ret i64 %.t916
}

define i1 @Option$$u64$$__is_some({ i64, [8 x i8] } %opt) {
entry:
  %opt.addr = alloca { i64, [8 x i8] }
  store { i64, [8 x i8] } %opt, ptr %opt.addr
  %.t917 = load { i64, [8 x i8] }, ptr %opt.addr
  %.t918 = extractvalue { i64, [8 x i8] } %.t917, 0
  switch i64 %.t918, label %match_arm921 [
    i64 0, label %match_arm920
  ]
match_arm920:
  %.t922 = alloca { i64, [8 x i8] }
  store { i64, [8 x i8] } %.t917, ptr %.t922
  %.t923 = getelementptr inbounds { i64, [8 x i8] }, ptr %.t922, i32 0, i32 1
  %.t924 = getelementptr inbounds i8, ptr %.t923, i32 0
  %.t925 = alloca i64
  %.t926 = load i64, ptr %.t924
  store i64 %.t926, ptr %.t925
  br label %match_merge919
match_arm921:
  br label %match_merge919
match_merge919:
  %.t927 = phi i1 [ 1, %match_arm920 ], [ 0, %match_arm921 ]
  ret i1 %.t927
}

define i1 @Option$$u64$$__is_none({ i64, [8 x i8] } %opt) {
entry:
  %opt.addr = alloca { i64, [8 x i8] }
  store { i64, [8 x i8] } %opt, ptr %opt.addr
  %.t928 = load { i64, [8 x i8] }, ptr %opt.addr
  %.t929 = extractvalue { i64, [8 x i8] } %.t928, 0
  switch i64 %.t929, label %match_arm932 [
    i64 0, label %match_arm931
  ]
match_arm931:
  %.t933 = alloca { i64, [8 x i8] }
  store { i64, [8 x i8] } %.t928, ptr %.t933
  %.t934 = getelementptr inbounds { i64, [8 x i8] }, ptr %.t933, i32 0, i32 1
  %.t935 = getelementptr inbounds i8, ptr %.t934, i32 0
  %.t936 = alloca i64
  %.t937 = load i64, ptr %.t935
  store i64 %.t937, ptr %.t936
  br label %match_merge930
match_arm932:
  br label %match_merge930
match_merge930:
  %.t938 = phi i1 [ 0, %match_arm931 ], [ 1, %match_arm932 ]
  ret i1 %.t938
}

define i64 @Option$$u64$$__unwrap_or({ i64, [8 x i8] } %opt, i64 %fallback) {
entry:
  %opt.addr = alloca { i64, [8 x i8] }
  store { i64, [8 x i8] } %opt, ptr %opt.addr
  %fallback.addr = alloca i64
  store i64 %fallback, ptr %fallback.addr
  %.t939 = load { i64, [8 x i8] }, ptr %opt.addr
  %.t940 = extractvalue { i64, [8 x i8] } %.t939, 0
  switch i64 %.t940, label %match_arm943 [
    i64 0, label %match_arm942
  ]
match_arm942:
  %.t944 = alloca { i64, [8 x i8] }
  store { i64, [8 x i8] } %.t939, ptr %.t944
  %.t945 = getelementptr inbounds { i64, [8 x i8] }, ptr %.t944, i32 0, i32 1
  %.t946 = getelementptr inbounds i8, ptr %.t945, i32 0
  %.t947 = alloca i64
  %.t948 = load i64, ptr %.t946
  store i64 %.t948, ptr %.t947
  %.t949 = load i64, ptr %.t947
  %.t950 = trunc i64 %.t949 to i32
  br label %match_merge941
match_arm943:
  %.t951 = load i64, ptr %fallback.addr
  %.t952 = trunc i64 %.t951 to i32
  br label %match_merge941
match_merge941:
  %.t953 = phi i32 [ %.t950, %match_arm942 ], [ %.t952, %match_arm943 ]
  %.t954 = sext i32 %.t953 to i64
  ret i64 %.t954
}

define i1 @Option$$u8$$__is_some({ i64, [1 x i8] } %opt) {
entry:
  %opt.addr = alloca { i64, [1 x i8] }
  store { i64, [1 x i8] } %opt, ptr %opt.addr
  %.t955 = load { i64, [1 x i8] }, ptr %opt.addr
  %.t956 = extractvalue { i64, [1 x i8] } %.t955, 0
  switch i64 %.t956, label %match_arm959 [
    i64 0, label %match_arm958
  ]
match_arm958:
  %.t960 = alloca { i64, [1 x i8] }
  store { i64, [1 x i8] } %.t955, ptr %.t960
  %.t961 = getelementptr inbounds { i64, [1 x i8] }, ptr %.t960, i32 0, i32 1
  %.t962 = getelementptr inbounds i8, ptr %.t961, i32 0
  %.t963 = alloca i8
  %.t964 = load i8, ptr %.t962
  store i8 %.t964, ptr %.t963
  br label %match_merge957
match_arm959:
  br label %match_merge957
match_merge957:
  %.t965 = phi i1 [ 1, %match_arm958 ], [ 0, %match_arm959 ]
  ret i1 %.t965
}

define i1 @Option$$u8$$__is_none({ i64, [1 x i8] } %opt) {
entry:
  %opt.addr = alloca { i64, [1 x i8] }
  store { i64, [1 x i8] } %opt, ptr %opt.addr
  %.t966 = load { i64, [1 x i8] }, ptr %opt.addr
  %.t967 = extractvalue { i64, [1 x i8] } %.t966, 0
  switch i64 %.t967, label %match_arm970 [
    i64 0, label %match_arm969
  ]
match_arm969:
  %.t971 = alloca { i64, [1 x i8] }
  store { i64, [1 x i8] } %.t966, ptr %.t971
  %.t972 = getelementptr inbounds { i64, [1 x i8] }, ptr %.t971, i32 0, i32 1
  %.t973 = getelementptr inbounds i8, ptr %.t972, i32 0
  %.t974 = alloca i8
  %.t975 = load i8, ptr %.t973
  store i8 %.t975, ptr %.t974
  br label %match_merge968
match_arm970:
  br label %match_merge968
match_merge968:
  %.t976 = phi i1 [ 0, %match_arm969 ], [ 1, %match_arm970 ]
  ret i1 %.t976
}

define i8 @Option$$u8$$__unwrap_or({ i64, [1 x i8] } %opt, i8 %fallback) {
entry:
  %opt.addr = alloca { i64, [1 x i8] }
  store { i64, [1 x i8] } %opt, ptr %opt.addr
  %fallback.addr = alloca i8
  store i8 %fallback, ptr %fallback.addr
  %.t977 = load { i64, [1 x i8] }, ptr %opt.addr
  %.t978 = extractvalue { i64, [1 x i8] } %.t977, 0
  switch i64 %.t978, label %match_arm981 [
    i64 0, label %match_arm980
  ]
match_arm980:
  %.t982 = alloca { i64, [1 x i8] }
  store { i64, [1 x i8] } %.t977, ptr %.t982
  %.t983 = getelementptr inbounds { i64, [1 x i8] }, ptr %.t982, i32 0, i32 1
  %.t984 = getelementptr inbounds i8, ptr %.t983, i32 0
  %.t985 = alloca i8
  %.t986 = load i8, ptr %.t984
  store i8 %.t986, ptr %.t985
  %.t987 = load i8, ptr %.t985
  %.t988 = zext i8 %.t987 to i32
  br label %match_merge979
match_arm981:
  %.t989 = load i8, ptr %fallback.addr
  %.t990 = zext i8 %.t989 to i32
  br label %match_merge979
match_merge979:
  %.t991 = phi i32 [ %.t988, %match_arm980 ], [ %.t990, %match_arm981 ]
  %.t992 = trunc i32 %.t991 to i8
  ret i8 %.t992
}

define i1 @Option$$ptr$$__is_some({ i64, [8 x i8] } %opt) {
entry:
  %opt.addr = alloca { i64, [8 x i8] }
  store { i64, [8 x i8] } %opt, ptr %opt.addr
  %.t993 = load { i64, [8 x i8] }, ptr %opt.addr
  %.t994 = extractvalue { i64, [8 x i8] } %.t993, 0
  switch i64 %.t994, label %match_arm997 [
    i64 0, label %match_arm996
  ]
match_arm996:
  %.t998 = alloca { i64, [8 x i8] }
  store { i64, [8 x i8] } %.t993, ptr %.t998
  %.t999 = getelementptr inbounds { i64, [8 x i8] }, ptr %.t998, i32 0, i32 1
  %.t1000 = getelementptr inbounds i8, ptr %.t999, i32 0
  %.t1001 = alloca ptr
  %.t1002 = load ptr, ptr %.t1000
  store ptr %.t1002, ptr %.t1001
  br label %match_merge995
match_arm997:
  br label %match_merge995
match_merge995:
  %.t1003 = phi i1 [ 1, %match_arm996 ], [ 0, %match_arm997 ]
  ret i1 %.t1003
}

define i1 @Option$$ptr$$__is_none({ i64, [8 x i8] } %opt) {
entry:
  %opt.addr = alloca { i64, [8 x i8] }
  store { i64, [8 x i8] } %opt, ptr %opt.addr
  %.t1004 = load { i64, [8 x i8] }, ptr %opt.addr
  %.t1005 = extractvalue { i64, [8 x i8] } %.t1004, 0
  switch i64 %.t1005, label %match_arm1008 [
    i64 0, label %match_arm1007
  ]
match_arm1007:
  %.t1009 = alloca { i64, [8 x i8] }
  store { i64, [8 x i8] } %.t1004, ptr %.t1009
  %.t1010 = getelementptr inbounds { i64, [8 x i8] }, ptr %.t1009, i32 0, i32 1
  %.t1011 = getelementptr inbounds i8, ptr %.t1010, i32 0
  %.t1012 = alloca ptr
  %.t1013 = load ptr, ptr %.t1011
  store ptr %.t1013, ptr %.t1012
  br label %match_merge1006
match_arm1008:
  br label %match_merge1006
match_merge1006:
  %.t1014 = phi i1 [ 0, %match_arm1007 ], [ 1, %match_arm1008 ]
  ret i1 %.t1014
}

define ptr @Option$$ptr$$__unwrap_or({ i64, [8 x i8] } %opt, ptr %fallback) {
entry:
  %opt.addr = alloca { i64, [8 x i8] }
  store { i64, [8 x i8] } %opt, ptr %opt.addr
  %fallback.addr = alloca ptr
  store ptr %fallback, ptr %fallback.addr
  %.t1015 = load { i64, [8 x i8] }, ptr %opt.addr
  %.t1016 = extractvalue { i64, [8 x i8] } %.t1015, 0
  switch i64 %.t1016, label %match_arm1019 [
    i64 0, label %match_arm1018
  ]
match_arm1018:
  %.t1020 = alloca { i64, [8 x i8] }
  store { i64, [8 x i8] } %.t1015, ptr %.t1020
  %.t1021 = getelementptr inbounds { i64, [8 x i8] }, ptr %.t1020, i32 0, i32 1
  %.t1022 = getelementptr inbounds i8, ptr %.t1021, i32 0
  %.t1023 = alloca ptr
  %.t1024 = load ptr, ptr %.t1022
  store ptr %.t1024, ptr %.t1023
  %.t1025 = load ptr, ptr %.t1023
  %.t1026 = ptrtoint ptr %.t1025 to i32
  br label %match_merge1017
match_arm1019:
  %.t1027 = load ptr, ptr %fallback.addr
  %.t1028 = ptrtoint ptr %.t1027 to i32
  br label %match_merge1017
match_merge1017:
  %.t1029 = phi i32 [ %.t1026, %match_arm1018 ], [ %.t1028, %match_arm1019 ]
  %.t1030 = inttoptr i32 %.t1029 to ptr
  ret ptr %.t1030
}

define i1 @Option$$str$$__is_some({ i64, [8 x i8] } %opt) {
entry:
  %opt.addr = alloca { i64, [8 x i8] }
  store { i64, [8 x i8] } %opt, ptr %opt.addr
  %.t1031 = load { i64, [8 x i8] }, ptr %opt.addr
  %.t1032 = extractvalue { i64, [8 x i8] } %.t1031, 0
  switch i64 %.t1032, label %match_arm1035 [
    i64 0, label %match_arm1034
  ]
match_arm1034:
  %.t1036 = alloca { i64, [8 x i8] }
  store { i64, [8 x i8] } %.t1031, ptr %.t1036
  %.t1037 = getelementptr inbounds { i64, [8 x i8] }, ptr %.t1036, i32 0, i32 1
  %.t1038 = getelementptr inbounds i8, ptr %.t1037, i32 0
  %.t1039 = alloca ptr
  %.t1040 = load ptr, ptr %.t1038
  store ptr %.t1040, ptr %.t1039
  br label %match_merge1033
match_arm1035:
  br label %match_merge1033
match_merge1033:
  %.t1041 = phi i1 [ 1, %match_arm1034 ], [ 0, %match_arm1035 ]
  ret i1 %.t1041
}

define i1 @Option$$str$$__is_none({ i64, [8 x i8] } %opt) {
entry:
  %opt.addr = alloca { i64, [8 x i8] }
  store { i64, [8 x i8] } %opt, ptr %opt.addr
  %.t1042 = load { i64, [8 x i8] }, ptr %opt.addr
  %.t1043 = extractvalue { i64, [8 x i8] } %.t1042, 0
  switch i64 %.t1043, label %match_arm1046 [
    i64 0, label %match_arm1045
  ]
match_arm1045:
  %.t1047 = alloca { i64, [8 x i8] }
  store { i64, [8 x i8] } %.t1042, ptr %.t1047
  %.t1048 = getelementptr inbounds { i64, [8 x i8] }, ptr %.t1047, i32 0, i32 1
  %.t1049 = getelementptr inbounds i8, ptr %.t1048, i32 0
  %.t1050 = alloca ptr
  %.t1051 = load ptr, ptr %.t1049
  store ptr %.t1051, ptr %.t1050
  br label %match_merge1044
match_arm1046:
  br label %match_merge1044
match_merge1044:
  %.t1052 = phi i1 [ 0, %match_arm1045 ], [ 1, %match_arm1046 ]
  ret i1 %.t1052
}

define ptr @Option$$str$$__unwrap_or({ i64, [8 x i8] } %opt, ptr %fallback) {
entry:
  %opt.addr = alloca { i64, [8 x i8] }
  store { i64, [8 x i8] } %opt, ptr %opt.addr
  %fallback.addr = alloca ptr
  store ptr %fallback, ptr %fallback.addr
  %.t1053 = load { i64, [8 x i8] }, ptr %opt.addr
  %.t1054 = extractvalue { i64, [8 x i8] } %.t1053, 0
  switch i64 %.t1054, label %match_arm1057 [
    i64 0, label %match_arm1056
  ]
match_arm1056:
  %.t1058 = alloca { i64, [8 x i8] }
  store { i64, [8 x i8] } %.t1053, ptr %.t1058
  %.t1059 = getelementptr inbounds { i64, [8 x i8] }, ptr %.t1058, i32 0, i32 1
  %.t1060 = getelementptr inbounds i8, ptr %.t1059, i32 0
  %.t1061 = alloca ptr
  %.t1062 = load ptr, ptr %.t1060
  store ptr %.t1062, ptr %.t1061
  %.t1063 = load ptr, ptr %.t1061
  %.t1064 = ptrtoint ptr %.t1063 to i32
  br label %match_merge1055
match_arm1057:
  %.t1065 = load ptr, ptr %fallback.addr
  %.t1066 = ptrtoint ptr %.t1065 to i32
  br label %match_merge1055
match_merge1055:
  %.t1067 = phi i32 [ %.t1064, %match_arm1056 ], [ %.t1066, %match_arm1057 ]
  %.t1068 = inttoptr i32 %.t1067 to ptr
  ret ptr %.t1068
}

define i1 @Option$$bool$$__is_some({ i64, [0 x i8] } %opt) {
entry:
  %opt.addr = alloca { i64, [0 x i8] }
  store { i64, [0 x i8] } %opt, ptr %opt.addr
  %.t1069 = load { i64, [0 x i8] }, ptr %opt.addr
  %.t1070 = extractvalue { i64, [0 x i8] } %.t1069, 0
  switch i64 %.t1070, label %match_arm1073 [
    i64 0, label %match_arm1072
  ]
match_arm1072:
  %.t1074 = alloca { i64, [0 x i8] }
  store { i64, [0 x i8] } %.t1069, ptr %.t1074
  %.t1075 = getelementptr inbounds { i64, [0 x i8] }, ptr %.t1074, i32 0, i32 1
  %.t1076 = getelementptr inbounds i8, ptr %.t1075, i32 0
  %.t1077 = alloca i1
  %.t1078 = load i1, ptr %.t1076
  store i1 %.t1078, ptr %.t1077
  br label %match_merge1071
match_arm1073:
  br label %match_merge1071
match_merge1071:
  %.t1079 = phi i1 [ 1, %match_arm1072 ], [ 0, %match_arm1073 ]
  ret i1 %.t1079
}

define i1 @Option$$bool$$__is_none({ i64, [0 x i8] } %opt) {
entry:
  %opt.addr = alloca { i64, [0 x i8] }
  store { i64, [0 x i8] } %opt, ptr %opt.addr
  %.t1080 = load { i64, [0 x i8] }, ptr %opt.addr
  %.t1081 = extractvalue { i64, [0 x i8] } %.t1080, 0
  switch i64 %.t1081, label %match_arm1084 [
    i64 0, label %match_arm1083
  ]
match_arm1083:
  %.t1085 = alloca { i64, [0 x i8] }
  store { i64, [0 x i8] } %.t1080, ptr %.t1085
  %.t1086 = getelementptr inbounds { i64, [0 x i8] }, ptr %.t1085, i32 0, i32 1
  %.t1087 = getelementptr inbounds i8, ptr %.t1086, i32 0
  %.t1088 = alloca i1
  %.t1089 = load i1, ptr %.t1087
  store i1 %.t1089, ptr %.t1088
  br label %match_merge1082
match_arm1084:
  br label %match_merge1082
match_merge1082:
  %.t1090 = phi i1 [ 0, %match_arm1083 ], [ 1, %match_arm1084 ]
  ret i1 %.t1090
}

define i1 @Option$$bool$$__unwrap_or({ i64, [0 x i8] } %opt, i1 %fallback) {
entry:
  %opt.addr = alloca { i64, [0 x i8] }
  store { i64, [0 x i8] } %opt, ptr %opt.addr
  %fallback.addr = alloca i1
  store i1 %fallback, ptr %fallback.addr
  %.t1091 = load { i64, [0 x i8] }, ptr %opt.addr
  %.t1092 = extractvalue { i64, [0 x i8] } %.t1091, 0
  switch i64 %.t1092, label %match_arm1095 [
    i64 0, label %match_arm1094
  ]
match_arm1094:
  %.t1096 = alloca { i64, [0 x i8] }
  store { i64, [0 x i8] } %.t1091, ptr %.t1096
  %.t1097 = getelementptr inbounds { i64, [0 x i8] }, ptr %.t1096, i32 0, i32 1
  %.t1098 = getelementptr inbounds i8, ptr %.t1097, i32 0
  %.t1099 = alloca i1
  %.t1100 = load i1, ptr %.t1098
  store i1 %.t1100, ptr %.t1099
  %.t1101 = load i1, ptr %.t1099
  %.t1102 = zext i1 %.t1101 to i32
  br label %match_merge1093
match_arm1095:
  %.t1103 = load i1, ptr %fallback.addr
  %.t1104 = zext i1 %.t1103 to i32
  br label %match_merge1093
match_merge1093:
  %.t1105 = phi i32 [ %.t1102, %match_arm1094 ], [ %.t1104, %match_arm1095 ]
  %.t1106 = trunc i32 %.t1105 to i1
  ret i1 %.t1106
}

define ptr @PageAllocator__alloc({ i64 } %self, i64 %num_pages) {
entry:
  %self.addr = alloca { i64 }
  store { i64 } %self, ptr %self.addr
  %num_pages.addr = alloca i64
  store i64 %num_pages, ptr %num_pages.addr
  %.t1107 = load { i64 }, ptr %self.addr
  %pa = alloca { i64 }
  store { i64 } %.t1107, ptr %pa
  %.t1108 = load i64, ptr %num_pages.addr
  %.t1109 = load { i64 }, ptr %pa
  %.t1110 = extractvalue { i64 } %.t1109, 0
  %.t1111 = mul i64 %.t1108, %.t1110
  %total_size = alloca i64
  store i64 %.t1111, ptr %total_size
  %.t1112 = sext i32 0 to i64
  %null_addr = alloca i64
  store i64 %.t1112, ptr %null_addr
  %.t1113 = load i64, ptr %null_addr
  %.t1114 = inttoptr i64 %.t1113 to ptr
  %addr = alloca ptr
  store ptr %.t1114, ptr %addr
  %prot = alloca i32
  store i32 3, ptr %prot
  %flags = alloca i32
  store i32 34, ptr %flags
  %.t1115 = sub i32 0, 1
  %fd = alloca i32
  store i32 %.t1115, ptr %fd
  %.t1116 = sext i32 0 to i64
  %offset = alloca i64
  store i64 %.t1116, ptr %offset
  %.t1117 = load ptr, ptr %addr
  %.t1118 = load i64, ptr %total_size
  %.t1119 = load i32, ptr %prot
  %.t1120 = load i32, ptr %flags
  %.t1121 = load i32, ptr %fd
  %.t1122 = load i64, ptr %offset
  %.t1123 = call ptr @mmap(ptr %.t1117, i64 %.t1118, i32 %.t1119, i32 %.t1120, i32 %.t1121, i64 %.t1122)
  %ptr = alloca ptr
  store ptr %.t1123, ptr %ptr
  %.t1124 = load ptr, ptr %ptr
  ret ptr %.t1124
}

define {  } @PageAllocator__free({ i64 } %self, ptr %ptr, i64 %num_pages) {
entry:
  %self.addr = alloca { i64 }
  store { i64 } %self, ptr %self.addr
  %ptr.addr = alloca ptr
  store ptr %ptr, ptr %ptr.addr
  %num_pages.addr = alloca i64
  store i64 %num_pages, ptr %num_pages.addr
  %.t1125 = load { i64 }, ptr %self.addr
  %pa = alloca { i64 }
  store { i64 } %.t1125, ptr %pa
  %.t1126 = load i64, ptr %num_pages.addr
  %.t1127 = load { i64 }, ptr %pa
  %.t1128 = extractvalue { i64 } %.t1127, 0
  %.t1129 = mul i64 %.t1126, %.t1128
  %total_size = alloca i64
  store i64 %.t1129, ptr %total_size
  %.t1130 = load ptr, ptr %ptr.addr
  %.t1131 = load i64, ptr %total_size
  %.t1132 = call i32 @munmap(ptr %.t1130, i64 %.t1131)
  %result = alloca i32
  store i32 %.t1132, ptr %result
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
  %.t1133 = load { i64 }, ptr %self.addr
  %pa = alloca { i64 }
  store { i64 } %.t1133, ptr %pa
  %.t1134 = load { i64 }, ptr %pa
  %.t1135 = load i64, ptr %new_pages.addr
  %.t1136 = call ptr @PageAllocator__alloc({ i64 } %.t1134, i64 %.t1135)
  %new_ptr = alloca ptr
  store ptr %.t1136, ptr %new_ptr
  %.t1137 = load i64, ptr %old_pages.addr
  %.t1138 = load { i64 }, ptr %pa
  %.t1139 = extractvalue { i64 } %.t1138, 0
  %.t1140 = mul i64 %.t1137, %.t1139
  %old_size = alloca i64
  store i64 %.t1140, ptr %old_size
  %.t1141 = load ptr, ptr %new_ptr
  %.t1142 = load ptr, ptr %ptr.addr
  %.t1143 = load i64, ptr %old_size
  %.t1144 = call {  } @memcpy(ptr %.t1141, ptr %.t1142, i64 %.t1143)
  %.t1145 = load { i64 }, ptr %pa
  %.t1146 = load ptr, ptr %ptr.addr
  %.t1147 = load i64, ptr %old_pages.addr
  %.t1148 = call {  } @PageAllocator__free({ i64 } %.t1145, ptr %.t1146, i64 %.t1147)
  %.t1149 = load ptr, ptr %new_ptr
  ret ptr %.t1149
}

define { ptr, { { i64 }, ptr, i64, i64, i64 } } @Arena__alloc({ { i64 }, ptr, i64, i64, i64 } %self, i64 %size) {
entry:
  %self.addr = alloca { { i64 }, ptr, i64, i64, i64 }
  store { { i64 }, ptr, i64, i64, i64 } %self, ptr %self.addr
  %size.addr = alloca i64
  store i64 %size, ptr %size.addr
  %.t1150 = load { { i64 }, ptr, i64, i64, i64 }, ptr %self.addr
  %arena = alloca { { i64 }, ptr, i64, i64, i64 }
  store { { i64 }, ptr, i64, i64, i64 } %.t1150, ptr %arena
  %.t1151 = load { { i64 }, ptr, i64, i64, i64 }, ptr %arena
  %.t1152 = extractvalue { { i64 }, ptr, i64, i64, i64 } %.t1151, 3
  %.t1153 = load i64, ptr %size.addr
  %.t1154 = add i64 %.t1152, %.t1153
  %new_used = alloca i64
  store i64 %.t1154, ptr %new_used
  %.t1155 = load i64, ptr %new_used
  %.t1156 = load { { i64 }, ptr, i64, i64, i64 }, ptr %arena
  %.t1157 = extractvalue { { i64 }, ptr, i64, i64, i64 } %.t1156, 2
  %.t1158 = icmp ugt i64 %.t1155, %.t1157
  %needs_grow = alloca i1
  store i1 %.t1158, ptr %needs_grow
  %.t1159 = load { { i64 }, ptr, i64, i64, i64 }, ptr %arena
  %arena_updated = alloca { { i64 }, ptr, i64, i64, i64 }
  store { { i64 }, ptr, i64, i64, i64 } %.t1159, ptr %arena_updated
  %.t1163 = load i1, ptr %needs_grow
  br i1 %.t1163, label %if_then1160, label %if_end1162
if_then1160:
  %.t1164 = load { { i64 }, ptr, i64, i64, i64 }, ptr %arena
  %.t1165 = extractvalue { { i64 }, ptr, i64, i64, i64 } %.t1164, 0
  %.t1166 = extractvalue { i64 } %.t1165, 0
  %page_size = alloca i64
  store i64 %.t1166, ptr %page_size
  %.t1167 = load i64, ptr %new_used
  %.t1168 = load i64, ptr %page_size
  %.t1169 = add i64 %.t1167, %.t1168
  %.t1170 = sub i64 %.t1169, 1
  %.t1171 = load i64, ptr %page_size
  %.t1172 = udiv i64 %.t1170, %.t1171
  %total_pages_needed = alloca i64
  store i64 %.t1172, ptr %total_pages_needed
  %.t1173 = load { { i64 }, ptr, i64, i64, i64 }, ptr %arena
  %.t1174 = extractvalue { { i64 }, ptr, i64, i64, i64 } %.t1173, 0
  %.t1175 = load i64, ptr %total_pages_needed
  %.t1176 = call ptr @PageAllocator__alloc({ i64 } %.t1174, i64 %.t1175)
  %new_buffer = alloca ptr
  store ptr %.t1176, ptr %new_buffer
  %.t1177 = load i64, ptr %total_pages_needed
  %.t1178 = load i64, ptr %page_size
  %.t1179 = mul i64 %.t1177, %.t1178
  %new_capacity = alloca i64
  store i64 %.t1179, ptr %new_capacity
  %.t1180 = load { { i64 }, ptr, i64, i64, i64 }, ptr %arena
  %.t1181 = extractvalue { { i64 }, ptr, i64, i64, i64 } %.t1180, 3
  %.t1182 = icmp ugt i64 %.t1181, 0
  %has_old_data = alloca i1
  store i1 %.t1182, ptr %has_old_data
  %.t1186 = load i1, ptr %has_old_data
  br i1 %.t1186, label %if_then1183, label %if_end1185
if_then1183:
  %.t1187 = load ptr, ptr %new_buffer
  %.t1188 = load { { i64 }, ptr, i64, i64, i64 }, ptr %arena
  %.t1189 = extractvalue { { i64 }, ptr, i64, i64, i64 } %.t1188, 1
  %.t1190 = load { { i64 }, ptr, i64, i64, i64 }, ptr %arena
  %.t1191 = extractvalue { { i64 }, ptr, i64, i64, i64 } %.t1190, 3
  %.t1192 = call {  } @memcpy(ptr %.t1187, ptr %.t1189, i64 %.t1191)
  %copy_result = alloca {  }
  store {  } %.t1192, ptr %copy_result
  %.t1193 = load { { i64 }, ptr, i64, i64, i64 }, ptr %arena
  %.t1194 = extractvalue { { i64 }, ptr, i64, i64, i64 } %.t1193, 0
  %.t1195 = load { { i64 }, ptr, i64, i64, i64 }, ptr %arena
  %.t1196 = extractvalue { { i64 }, ptr, i64, i64, i64 } %.t1195, 1
  %.t1197 = load { { i64 }, ptr, i64, i64, i64 }, ptr %arena
  %.t1198 = extractvalue { { i64 }, ptr, i64, i64, i64 } %.t1197, 4
  %.t1199 = call {  } @PageAllocator__free({ i64 } %.t1194, ptr %.t1196, i64 %.t1198)
  %free_result = alloca {  }
  store {  } %.t1199, ptr %free_result
  br label %if_end1185
if_end1185:
  %.t1200 = load { { i64 }, ptr, i64, i64, i64 }, ptr %arena
  %.t1201 = extractvalue { { i64 }, ptr, i64, i64, i64 } %.t1200, 0
  %.t1202 = insertvalue { { i64 }, ptr, i64, i64, i64 } undef, { i64 } %.t1201, 0
  %.t1203 = load ptr, ptr %new_buffer
  %.t1204 = insertvalue { { i64 }, ptr, i64, i64, i64 } %.t1202, ptr %.t1203, 1
  %.t1205 = load i64, ptr %new_capacity
  %.t1206 = insertvalue { { i64 }, ptr, i64, i64, i64 } %.t1204, i64 %.t1205, 2
  %.t1207 = load { { i64 }, ptr, i64, i64, i64 }, ptr %arena
  %.t1208 = extractvalue { { i64 }, ptr, i64, i64, i64 } %.t1207, 3
  %.t1209 = insertvalue { { i64 }, ptr, i64, i64, i64 } %.t1206, i64 %.t1208, 3
  %.t1210 = load i64, ptr %total_pages_needed
  %.t1211 = insertvalue { { i64 }, ptr, i64, i64, i64 } %.t1209, i64 %.t1210, 4
  %new_arena = alloca { { i64 }, ptr, i64, i64, i64 }
  store { { i64 }, ptr, i64, i64, i64 } %.t1211, ptr %new_arena
  %.t1212 = load { { i64 }, ptr, i64, i64, i64 }, ptr %new_arena
  store { { i64 }, ptr, i64, i64, i64 } %.t1212, ptr %arena_updated
  br label %if_end1162
if_end1162:
  %.t1213 = load { { i64 }, ptr, i64, i64, i64 }, ptr %arena_updated
  %.t1214 = extractvalue { { i64 }, ptr, i64, i64, i64 } %.t1213, 1
  %.t1215 = load { { i64 }, ptr, i64, i64, i64 }, ptr %arena_updated
  %.t1216 = extractvalue { { i64 }, ptr, i64, i64, i64 } %.t1215, 3
  %.t1217 = getelementptr i64, ptr %.t1214, i64 %.t1216
  %alloc_ptr = alloca ptr
  store ptr %.t1217, ptr %alloc_ptr
  %.t1218 = load { { i64 }, ptr, i64, i64, i64 }, ptr %arena_updated
  %.t1219 = extractvalue { { i64 }, ptr, i64, i64, i64 } %.t1218, 0
  %.t1220 = insertvalue { { i64 }, ptr, i64, i64, i64 } undef, { i64 } %.t1219, 0
  %.t1221 = load { { i64 }, ptr, i64, i64, i64 }, ptr %arena_updated
  %.t1222 = extractvalue { { i64 }, ptr, i64, i64, i64 } %.t1221, 1
  %.t1223 = insertvalue { { i64 }, ptr, i64, i64, i64 } %.t1220, ptr %.t1222, 1
  %.t1224 = load { { i64 }, ptr, i64, i64, i64 }, ptr %arena_updated
  %.t1225 = extractvalue { { i64 }, ptr, i64, i64, i64 } %.t1224, 2
  %.t1226 = insertvalue { { i64 }, ptr, i64, i64, i64 } %.t1223, i64 %.t1225, 2
  %.t1227 = load i64, ptr %new_used
  %.t1228 = insertvalue { { i64 }, ptr, i64, i64, i64 } %.t1226, i64 %.t1227, 3
  %.t1229 = load { { i64 }, ptr, i64, i64, i64 }, ptr %arena_updated
  %.t1230 = extractvalue { { i64 }, ptr, i64, i64, i64 } %.t1229, 4
  %.t1231 = insertvalue { { i64 }, ptr, i64, i64, i64 } %.t1228, i64 %.t1230, 4
  %final_arena = alloca { { i64 }, ptr, i64, i64, i64 }
  store { { i64 }, ptr, i64, i64, i64 } %.t1231, ptr %final_arena
  %.t1232 = load ptr, ptr %alloc_ptr
  %.t1233 = insertvalue { ptr, { { i64 }, ptr, i64, i64, i64 } } undef, ptr %.t1232, 0
  %.t1234 = load { { i64 }, ptr, i64, i64, i64 }, ptr %final_arena
  %.t1235 = insertvalue { ptr, { { i64 }, ptr, i64, i64, i64 } } %.t1233, { { i64 }, ptr, i64, i64, i64 } %.t1234, 1
  ret { ptr, { { i64 }, ptr, i64, i64, i64 } } %.t1235
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
  %.t1236 = load { { i64 }, ptr, i64, i64, i64 }, ptr %self.addr
  %arena = alloca { { i64 }, ptr, i64, i64, i64 }
  store { { i64 }, ptr, i64, i64, i64 } %.t1236, ptr %arena
  %.t1237 = load { { i64 }, ptr, i64, i64, i64 }, ptr %arena
  %.t1238 = extractvalue { { i64 }, ptr, i64, i64, i64 } %.t1237, 1
  ret ptr %.t1238
}

define { ptr, { { i64 }, ptr, ptr, ptr } } @GeneralPurposeAllocator__alloc({ { i64 }, ptr, ptr, ptr } %self, i64 %size) {
entry:
  %self.addr = alloca { { i64 }, ptr, ptr, ptr }
  store { { i64 }, ptr, ptr, ptr } %self, ptr %self.addr
  %size.addr = alloca i64
  store i64 %size, ptr %size.addr
  %.t1239 = load { { i64 }, ptr, ptr, ptr }, ptr %self.addr
  %gpa = alloca { { i64 }, ptr, ptr, ptr }
  store { { i64 }, ptr, ptr, ptr } %.t1239, ptr %gpa
  %.t1240 = sext i32 16 to i64
  %header_size = alloca i64
  store i64 %.t1240, ptr %header_size
  %.t1241 = load i64, ptr %size.addr
  %.t1242 = load i64, ptr %header_size
  %.t1243 = add i64 %.t1241, %.t1242
  %total_size = alloca i64
  store i64 %.t1243, ptr %total_size
  %.t1244 = sext i32 0 to i64
  %null_addr = alloca i64
  store i64 %.t1244, ptr %null_addr
  %.t1245 = load i64, ptr %null_addr
  %.t1246 = inttoptr i64 %.t1245 to ptr
  %null_ptr = alloca ptr
  store ptr %.t1246, ptr %null_ptr
  %.t1247 = load { { i64 }, ptr, ptr, ptr }, ptr %gpa
  %.t1248 = extractvalue { { i64 }, ptr, ptr, ptr } %.t1247, 1
  %current = alloca ptr
  store ptr %.t1248, ptr %current
  %.t1249 = load ptr, ptr %null_ptr
  %prev = alloca ptr
  store ptr %.t1249, ptr %prev
  %.t1250 = load ptr, ptr %null_ptr
  %found_block = alloca ptr
  store ptr %.t1250, ptr %found_block
  %.t1251 = load ptr, ptr %null_ptr
  %found_prev = alloca ptr
  store ptr %.t1251, ptr %found_prev
  br label %while_header1252
while_header1252:
  %.t1255 = load ptr, ptr %current
  %.t1256 = load ptr, ptr %null_ptr
  %.t1257 = icmp ne ptr %.t1255, %.t1256
  br i1 %.t1257, label %while_body1253, label %while_exit1254
while_body1253:
  %.t1258 = load ptr, ptr %current
  %.t1259 = load i64, ptr %.t1258
  %block_size = alloca i64
  store i64 %.t1259, ptr %block_size
  %.t1260 = load i64, ptr %block_size
  %.t1261 = load i64, ptr %total_size
  %.t1262 = icmp uge i64 %.t1260, %.t1261
  %fits = alloca i1
  store i1 %.t1262, ptr %fits
  %.t1263 = load i1, ptr %fits
  br i1 %.t1263, label %then1264, label %else1265
then1264:
  %.t1267 = load ptr, ptr %current
  store ptr %.t1267, ptr %found_block
  %.t1268 = load ptr, ptr %prev
  store ptr %.t1268, ptr %found_prev
  %.t1269 = load ptr, ptr %null_ptr
  store ptr %.t1269, ptr %current
  br label %merge1266
else1265:
  %.t1270 = load ptr, ptr %current
  %.t1271 = getelementptr i64, ptr %.t1270, i64 8
  %current_next_offset = alloca ptr
  store ptr %.t1271, ptr %current_next_offset
  %.t1272 = load ptr, ptr %current_next_offset
  %.t1273 = load ptr, ptr %.t1272
  %next = alloca ptr
  store ptr %.t1273, ptr %next
  %.t1274 = load ptr, ptr %current
  store ptr %.t1274, ptr %prev
  %.t1275 = load ptr, ptr %next
  store ptr %.t1275, ptr %current
  br label %merge1266
merge1266:
  %.t1276 = phi i32 [ 0, %then1264 ], [ 0, %else1265 ]
  br label %while_header1252
while_exit1254:
  %.t1277 = load ptr, ptr %found_block
  %.t1278 = load ptr, ptr %null_ptr
  %.t1279 = icmp ne ptr %.t1277, %.t1278
  br i1 %.t1279, label %then1280, label %else1281
then1280:
  %.t1283 = load ptr, ptr %found_block
  %.t1284 = getelementptr i64, ptr %.t1283, i64 8
  %next_offset2 = alloca ptr
  store ptr %.t1284, ptr %next_offset2
  %.t1285 = load ptr, ptr %next_offset2
  %.t1286 = load ptr, ptr %.t1285
  %next_block = alloca ptr
  store ptr %.t1286, ptr %next_block
  %.t1287 = load ptr, ptr %found_prev
  %.t1288 = load ptr, ptr %null_ptr
  %.t1289 = icmp eq ptr %.t1287, %.t1288
  br i1 %.t1289, label %then1290, label %else1291
then1290:
  %.t1293 = load ptr, ptr %next_block
  br label %merge1292
else1291:
  %.t1294 = load ptr, ptr %found_prev
  %.t1295 = getelementptr i64, ptr %.t1294, i64 8
  %prev_next_offset = alloca ptr
  store ptr %.t1295, ptr %prev_next_offset
  %.t1296 = load ptr, ptr %prev_next_offset
  %.t1297 = load ptr, ptr %next_block
  %.t1298 = ptrtoint ptr %.t1297 to i64
  store i64 %.t1298, ptr %.t1296
  %_w3 = alloca {  }
  store {  } undef, ptr %_w3
  %.t1299 = load { { i64 }, ptr, ptr, ptr }, ptr %gpa
  %.t1300 = extractvalue { { i64 }, ptr, ptr, ptr } %.t1299, 1
  br label %merge1292
merge1292:
  %.t1301 = phi ptr [ %.t1293, %then1290 ], [ %.t1300, %else1291 ]
  %new_free_list = alloca ptr
  store ptr %.t1301, ptr %new_free_list
  %.t1302 = load { { i64 }, ptr, ptr, ptr }, ptr %gpa
  %.t1303 = extractvalue { { i64 }, ptr, ptr, ptr } %.t1302, 0
  %.t1304 = insertvalue { { i64 }, ptr, ptr, ptr } undef, { i64 } %.t1303, 0
  %.t1305 = load ptr, ptr %new_free_list
  %.t1306 = insertvalue { { i64 }, ptr, ptr, ptr } %.t1304, ptr %.t1305, 1
  %.t1307 = load { { i64 }, ptr, ptr, ptr }, ptr %gpa
  %.t1308 = extractvalue { { i64 }, ptr, ptr, ptr } %.t1307, 2
  %.t1309 = insertvalue { { i64 }, ptr, ptr, ptr } %.t1306, ptr %.t1308, 2
  %.t1310 = load { { i64 }, ptr, ptr, ptr }, ptr %gpa
  %.t1311 = extractvalue { { i64 }, ptr, ptr, ptr } %.t1310, 3
  %.t1312 = insertvalue { { i64 }, ptr, ptr, ptr } %.t1309, ptr %.t1311, 3
  %updated_gpa = alloca { { i64 }, ptr, ptr, ptr }
  store { { i64 }, ptr, ptr, ptr } %.t1312, ptr %updated_gpa
  %.t1313 = load { { i64 }, ptr, ptr, ptr }, ptr %updated_gpa
  store { { i64 }, ptr, ptr, ptr } %.t1313, ptr %gpa
  %.t1314 = load ptr, ptr %found_block
  %.t1315 = load i64, ptr %header_size
  %.t1316 = getelementptr i64, ptr %.t1314, i64 %.t1315
  br label %merge1282
else1281:
  %.t1317 = load { { i64 }, ptr, ptr, ptr }, ptr %gpa
  %.t1318 = extractvalue { { i64 }, ptr, ptr, ptr } %.t1317, 0
  %.t1319 = extractvalue { i64 } %.t1318, 0
  %page_size = alloca i64
  store i64 %.t1319, ptr %page_size
  %.t1320 = load i64, ptr %total_size
  %.t1321 = load i64, ptr %page_size
  %.t1322 = add i64 %.t1320, %.t1321
  %.t1323 = sub i64 %.t1322, 1
  %.t1324 = load i64, ptr %page_size
  %.t1325 = udiv i64 %.t1323, %.t1324
  %pages_needed = alloca i64
  store i64 %.t1325, ptr %pages_needed
  %.t1326 = load { { i64 }, ptr, ptr, ptr }, ptr %gpa
  %.t1327 = extractvalue { { i64 }, ptr, ptr, ptr } %.t1326, 0
  %.t1328 = load i64, ptr %pages_needed
  %.t1329 = call ptr @PageAllocator__alloc({ i64 } %.t1327, i64 %.t1328)
  %new_block = alloca ptr
  store ptr %.t1329, ptr %new_block
  %.t1330 = load ptr, ptr %new_block
  %.t1331 = load i64, ptr %total_size
  store i64 %.t1331, ptr %.t1330
  %_w1 = alloca {  }
  store {  } undef, ptr %_w1
  %.t1332 = load ptr, ptr %new_block
  %.t1333 = getelementptr i64, ptr %.t1332, i64 8
  %new_block_next_offset = alloca ptr
  store ptr %.t1333, ptr %new_block_next_offset
  %.t1334 = load ptr, ptr %new_block_next_offset
  %.t1335 = load ptr, ptr %null_ptr
  %.t1336 = ptrtoint ptr %.t1335 to i64
  store i64 %.t1336, ptr %.t1334
  %_w2 = alloca {  }
  store {  } undef, ptr %_w2
  %.t1337 = load ptr, ptr %new_block
  %.t1338 = load i64, ptr %header_size
  %.t1339 = getelementptr i64, ptr %.t1337, i64 %.t1338
  br label %merge1282
merge1282:
  %.t1340 = phi ptr [ %.t1316, %merge1292 ], [ %.t1339, %else1281 ]
  %allocated_ptr = alloca ptr
  store ptr %.t1340, ptr %allocated_ptr
  %.t1341 = load { { i64 }, ptr, ptr, ptr }, ptr %gpa
  %final_gpa = alloca { { i64 }, ptr, ptr, ptr }
  store { { i64 }, ptr, ptr, ptr } %.t1341, ptr %final_gpa
  %.t1342 = load ptr, ptr %allocated_ptr
  %.t1343 = insertvalue { ptr, { { i64 }, ptr, ptr, ptr } } undef, ptr %.t1342, 0
  %.t1344 = load { { i64 }, ptr, ptr, ptr }, ptr %final_gpa
  %.t1345 = insertvalue { ptr, { { i64 }, ptr, ptr, ptr } } %.t1343, { { i64 }, ptr, ptr, ptr } %.t1344, 1
  ret { ptr, { { i64 }, ptr, ptr, ptr } } %.t1345
}

define { { i64 }, ptr, ptr, ptr } @GeneralPurposeAllocator__free({ { i64 }, ptr, ptr, ptr } %self, ptr %ptr) {
entry:
  %self.addr = alloca { { i64 }, ptr, ptr, ptr }
  store { { i64 }, ptr, ptr, ptr } %self, ptr %self.addr
  %ptr.addr = alloca ptr
  store ptr %ptr, ptr %ptr.addr
  %.t1346 = load { { i64 }, ptr, ptr, ptr }, ptr %self.addr
  %gpa = alloca { { i64 }, ptr, ptr, ptr }
  store { { i64 }, ptr, ptr, ptr } %.t1346, ptr %gpa
  %.t1347 = sext i32 16 to i64
  %header_size = alloca i64
  store i64 %.t1347, ptr %header_size
  %.t1348 = load ptr, ptr %ptr.addr
  %.t1349 = ptrtoint ptr %.t1348 to i64
  %ptr_as_int = alloca i64
  store i64 %.t1349, ptr %ptr_as_int
  %.t1350 = sext i32 16 to i64
  %header_size_as_int = alloca i64
  store i64 %.t1350, ptr %header_size_as_int
  %.t1351 = load i64, ptr %ptr_as_int
  %.t1352 = load i64, ptr %header_size_as_int
  %.t1353 = sub i64 %.t1351, %.t1352
  %block_start_int = alloca i64
  store i64 %.t1353, ptr %block_start_int
  %.t1354 = load i64, ptr %block_start_int
  %.t1355 = inttoptr i64 %.t1354 to ptr
  %block_start = alloca ptr
  store ptr %.t1355, ptr %block_start
  %.t1356 = load { { i64 }, ptr, ptr, ptr }, ptr %gpa
  %.t1357 = extractvalue { { i64 }, ptr, ptr, ptr } %.t1356, 1
  %old_head = alloca ptr
  store ptr %.t1357, ptr %old_head
  %.t1358 = load ptr, ptr %block_start
  %.t1359 = getelementptr i64, ptr %.t1358, i64 8
  %next_offset = alloca ptr
  store ptr %.t1359, ptr %next_offset
  %.t1360 = load ptr, ptr %next_offset
  %.t1361 = load ptr, ptr %old_head
  %.t1362 = ptrtoint ptr %.t1361 to i64
  store i64 %.t1362, ptr %.t1360
  %_w1 = alloca {  }
  store {  } undef, ptr %_w1
  %.t1363 = load { { i64 }, ptr, ptr, ptr }, ptr %gpa
  %.t1364 = extractvalue { { i64 }, ptr, ptr, ptr } %.t1363, 0
  %.t1365 = insertvalue { { i64 }, ptr, ptr, ptr } undef, { i64 } %.t1364, 0
  %.t1366 = load ptr, ptr %block_start
  %.t1367 = insertvalue { { i64 }, ptr, ptr, ptr } %.t1365, ptr %.t1366, 1
  %.t1368 = load { { i64 }, ptr, ptr, ptr }, ptr %gpa
  %.t1369 = extractvalue { { i64 }, ptr, ptr, ptr } %.t1368, 2
  %.t1370 = insertvalue { { i64 }, ptr, ptr, ptr } %.t1367, ptr %.t1369, 2
  %.t1371 = load { { i64 }, ptr, ptr, ptr }, ptr %gpa
  %.t1372 = extractvalue { { i64 }, ptr, ptr, ptr } %.t1371, 3
  %.t1373 = insertvalue { { i64 }, ptr, ptr, ptr } %.t1370, ptr %.t1372, 3
  ret { { i64 }, ptr, ptr, ptr } %.t1373
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
  %.t1374 = load { { i64 }, ptr, ptr, ptr }, ptr %self.addr
  %gpa = alloca { { i64 }, ptr, ptr, ptr }
  store { { i64 }, ptr, ptr, ptr } %.t1374, ptr %gpa
  %.t1375 = load ptr, ptr %ptr.addr
  %.t1376 = ptrtoint ptr %.t1375 to i64
  %ptr_as_int = alloca i64
  store i64 %.t1376, ptr %ptr_as_int
  %.t1377 = sext i32 16 to i64
  %header_size = alloca i64
  store i64 %.t1377, ptr %header_size
  %.t1378 = load i64, ptr %ptr_as_int
  %.t1379 = load i64, ptr %header_size
  %.t1380 = sub i64 %.t1378, %.t1379
  %block_start_int = alloca i64
  store i64 %.t1380, ptr %block_start_int
  %.t1381 = load i64, ptr %block_start_int
  %.t1382 = inttoptr i64 %.t1381 to ptr
  %block_start = alloca ptr
  store ptr %.t1382, ptr %block_start
  %.t1383 = load ptr, ptr %block_start
  %.t1384 = load i64, ptr %.t1383
  %old_total_size = alloca i64
  store i64 %.t1384, ptr %old_total_size
  %.t1385 = load i64, ptr %old_total_size
  %.t1386 = load i64, ptr %header_size
  %.t1387 = sub i64 %.t1385, %.t1386
  %old_size = alloca i64
  store i64 %.t1387, ptr %old_size
  %.t1388 = load { { i64 }, ptr, ptr, ptr }, ptr %gpa
  %.t1389 = load i64, ptr %new_size.addr
  %.t1390 = call { ptr, { { i64 }, ptr, ptr, ptr } } @GeneralPurposeAllocator__alloc({ { i64 }, ptr, ptr, ptr } %.t1388, i64 %.t1389)
  %alloc_result = alloca { ptr, { { i64 }, ptr, ptr, ptr } }
  store { ptr, { { i64 }, ptr, ptr, ptr } } %.t1390, ptr %alloc_result
  %.t1391 = load { ptr, { { i64 }, ptr, ptr, ptr } }, ptr %alloc_result
  %.t1392 = extractvalue { ptr, { { i64 }, ptr, ptr, ptr } } %.t1391, 0
  %new_ptr = alloca ptr
  store ptr %.t1392, ptr %new_ptr
  %.t1393 = load { ptr, { { i64 }, ptr, ptr, ptr } }, ptr %alloc_result
  %.t1394 = extractvalue { ptr, { { i64 }, ptr, ptr, ptr } } %.t1393, 1
  %gpa2 = alloca { { i64 }, ptr, ptr, ptr }
  store { { i64 }, ptr, ptr, ptr } %.t1394, ptr %gpa2
  %.t1395 = load i64, ptr %old_size
  %.t1396 = load i64, ptr %new_size.addr
  %.t1397 = icmp ult i64 %.t1395, %.t1396
  br i1 %.t1397, label %then1398, label %else1399
then1398:
  %.t1401 = load i64, ptr %old_size
  br label %merge1400
else1399:
  %.t1402 = load i64, ptr %new_size.addr
  br label %merge1400
merge1400:
  %.t1403 = phi i64 [ %.t1401, %then1398 ], [ %.t1402, %else1399 ]
  %copy_size = alloca i64
  store i64 %.t1403, ptr %copy_size
  %.t1404 = load ptr, ptr %new_ptr
  %.t1405 = load ptr, ptr %ptr.addr
  %.t1406 = load i64, ptr %copy_size
  %.t1407 = call {  } @memcpy(ptr %.t1404, ptr %.t1405, i64 %.t1406)
  %.t1408 = load { { i64 }, ptr, ptr, ptr }, ptr %gpa2
  %.t1409 = load ptr, ptr %ptr.addr
  %.t1410 = call { { i64 }, ptr, ptr, ptr } @GeneralPurposeAllocator__free({ { i64 }, ptr, ptr, ptr } %.t1408, ptr %.t1409)
  %gpa3 = alloca { { i64 }, ptr, ptr, ptr }
  store { { i64 }, ptr, ptr, ptr } %.t1410, ptr %gpa3
  %.t1411 = load ptr, ptr %new_ptr
  %.t1412 = insertvalue { ptr, { { i64 }, ptr, ptr, ptr } } undef, ptr %.t1411, 0
  %.t1413 = load { { i64 }, ptr, ptr, ptr }, ptr %gpa3
  %.t1414 = insertvalue { ptr, { { i64 }, ptr, ptr, ptr } } %.t1412, { { i64 }, ptr, ptr, ptr } %.t1413, 1
  ret { ptr, { { i64 }, ptr, ptr, ptr } } %.t1414
}

define i64 @str__len(ptr %self) {
entry:
  %self.addr = alloca ptr
  store ptr %self, ptr %self.addr
  %.t1415 = sext i32 0 to i64
  %length = alloca i64
  store i64 %.t1415, ptr %length
  %.t1416 = load ptr, ptr %self.addr
  %p = alloca ptr
  store ptr %.t1416, ptr %p
  br label %while_header1417
while_header1417:
  %.t1420 = load ptr, ptr %p
  %.t1421 = load i8, ptr %.t1420
  %.t1422 = zext i8 %.t1421 to i64
  %.t1423 = icmp ne i64 %.t1422, 0
  br i1 %.t1423, label %while_body1418, label %while_exit1419
while_body1418:
  %.t1424 = load i64, ptr %length
  %.t1425 = add i64 %.t1424, 1
  store i64 %.t1425, ptr %length
  %.t1426 = load ptr, ptr %p
  %.t1427 = getelementptr i64, ptr %.t1426, i64 1
  store ptr %.t1427, ptr %p
  br label %while_header1417
while_exit1419:
  %.t1428 = load i64, ptr %length
  ret i64 %.t1428
}

define i8 @str__at(ptr %self, i64 %index) {
entry:
  %self.addr = alloca ptr
  store ptr %self, ptr %self.addr
  %index.addr = alloca i64
  store i64 %index, ptr %index.addr
  %.t1429 = load ptr, ptr %self.addr
  %p = alloca ptr
  store ptr %.t1429, ptr %p
  %.t1430 = load ptr, ptr %p
  %.t1431 = load i64, ptr %index.addr
  %.t1432 = getelementptr i64, ptr %.t1430, i64 %.t1431
  store ptr %.t1432, ptr %p
  %.t1433 = load ptr, ptr %p
  %.t1434 = load i8, ptr %.t1433
  %.t1435 = zext i8 %.t1434 to i64
  %.t1436 = trunc i64 %.t1435 to i8
  ret i8 %.t1436
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
  %.t1437 = load i64, ptr %end.addr
  %.t1438 = load i64, ptr %start.addr
  %.t1439 = sub i64 %.t1437, %.t1438
  %slice_len = alloca i64
  store i64 %.t1439, ptr %slice_len
  %.t1440 = load i64, ptr %slice_len
  %.t1441 = icmp sle i64 %.t1440, 0
  %is_invalid = alloca i1
  store i1 %.t1441, ptr %is_invalid
  %.t1442 = load i1, ptr %is_invalid
  br i1 %.t1442, label %then1443, label %else1444
then1443:
  %.t1446 = insertvalue { ptr, { { i64 }, ptr, i64, i64, i64 } } undef, ptr getelementptr inbounds ([1 x i8], ptr @.str.18151106360299273605, i32 0, i32 0), 0
  %.t1447 = load { { i64 }, ptr, i64, i64, i64 }, ptr %arena.addr
  %.t1448 = insertvalue { ptr, { { i64 }, ptr, i64, i64, i64 } } %.t1446, { { i64 }, ptr, i64, i64, i64 } %.t1447, 1
  br label %merge1445
else1444:
  %.t1449 = load i64, ptr %slice_len
  %.t1450 = inttoptr i64 %.t1449 to ptr
  %len_ptr = alloca ptr
  store ptr %.t1450, ptr %len_ptr
  %.t1451 = load ptr, ptr %len_ptr
  %.t1452 = ptrtoint ptr %.t1451 to i64
  %len_u64 = alloca i64
  store i64 %.t1452, ptr %len_u64
  %.t1453 = load i64, ptr %len_u64
  %.t1454 = add i64 %.t1453, 1
  %alloc_size = alloca i64
  store i64 %.t1454, ptr %alloc_size
  %.t1455 = load { { i64 }, ptr, i64, i64, i64 }, ptr %arena.addr
  %.t1456 = load i64, ptr %alloc_size
  %.t1457 = call { ptr, { { i64 }, ptr, i64, i64, i64 } } @Arena__alloc({ { i64 }, ptr, i64, i64, i64 } %.t1455, i64 %.t1456)
  %alloc_result = alloca { ptr, { { i64 }, ptr, i64, i64, i64 } }
  store { ptr, { { i64 }, ptr, i64, i64, i64 } } %.t1457, ptr %alloc_result
  %.t1458 = load { ptr, { { i64 }, ptr, i64, i64, i64 } }, ptr %alloc_result
  %.t1459 = extractvalue { ptr, { { i64 }, ptr, i64, i64, i64 } } %.t1458, 0
  %buffer = alloca ptr
  store ptr %.t1459, ptr %buffer
  %.t1460 = load { ptr, { { i64 }, ptr, i64, i64, i64 } }, ptr %alloc_result
  %.t1461 = extractvalue { ptr, { { i64 }, ptr, i64, i64, i64 } } %.t1460, 1
  %arena2 = alloca { { i64 }, ptr, i64, i64, i64 }
  store { { i64 }, ptr, i64, i64, i64 } %.t1461, ptr %arena2
  %.t1462 = load ptr, ptr %self.addr
  %.t1463 = load i64, ptr %start.addr
  %.t1464 = getelementptr i64, ptr %.t1462, i64 %.t1463
  %src = alloca ptr
  store ptr %.t1464, ptr %src
  %.t1465 = load ptr, ptr %buffer
  %.t1466 = load ptr, ptr %src
  %.t1467 = load i64, ptr %len_u64
  %.t1468 = call {  } @memcpy(ptr %.t1465, ptr %.t1466, i64 %.t1467)
  %.t1469 = load ptr, ptr %buffer
  %.t1470 = load i64, ptr %len_u64
  %.t1471 = getelementptr i64, ptr %.t1469, i64 %.t1470
  %null_pos = alloca ptr
  store ptr %.t1471, ptr %null_pos
  %.t1472 = trunc i32 0 to i8
  %zero = alloca i8
  store i8 %.t1472, ptr %zero
  %.t1473 = load ptr, ptr %null_pos
  %.t1474 = load i8, ptr %zero
  store i8 %.t1474, ptr %.t1473
  %.t1475 = load ptr, ptr %buffer
  %.t1476 = insertvalue { ptr, { { i64 }, ptr, i64, i64, i64 } } undef, ptr %.t1475, 0
  %.t1477 = load { { i64 }, ptr, i64, i64, i64 }, ptr %arena2
  %.t1478 = insertvalue { ptr, { { i64 }, ptr, i64, i64, i64 } } %.t1476, { { i64 }, ptr, i64, i64, i64 } %.t1477, 1
  br label %merge1445
merge1445:
  %.t1479 = phi { ptr, { { i64 }, ptr, i64, i64, i64 } } [ %.t1448, %then1443 ], [ %.t1478, %else1444 ]
  ret { ptr, { { i64 }, ptr, i64, i64, i64 } } %.t1479
}

define i1 @str__is_empty(ptr %self) {
entry:
  %self.addr = alloca ptr
  store ptr %self, ptr %self.addr
  %.t1480 = load ptr, ptr %self.addr
  %p = alloca ptr
  store ptr %.t1480, ptr %p
  %.t1481 = load ptr, ptr %p
  %.t1482 = load i8, ptr %.t1481
  %.t1483 = zext i8 %.t1482 to i64
  %.t1484 = trunc i64 %.t1483 to i8
  %first = alloca i8
  store i8 %.t1484, ptr %first
  %.t1485 = load i8, ptr %first
  %.t1486 = icmp eq i8 %.t1485, 0
  ret i1 %.t1486
}

define i1 @str__equals(ptr %self, ptr %other) {
entry:
  %self.addr = alloca ptr
  store ptr %self, ptr %self.addr
  %other.addr = alloca ptr
  store ptr %other, ptr %other.addr
  %.t1487 = load ptr, ptr %self.addr
  %p1 = alloca ptr
  store ptr %.t1487, ptr %p1
  %.t1488 = load ptr, ptr %other.addr
  %p2 = alloca ptr
  store ptr %.t1488, ptr %p2
  %continue = alloca i1
  store i1 1, ptr %continue
  %result = alloca i1
  store i1 1, ptr %result
  br label %while_header1489
while_header1489:
  %.t1492 = load i1, ptr %continue
  br i1 %.t1492, label %while_body1490, label %while_exit1491
while_body1490:
  %.t1493 = load ptr, ptr %p1
  %.t1494 = load i8, ptr %.t1493
  %.t1495 = zext i8 %.t1494 to i64
  %.t1496 = trunc i64 %.t1495 to i8
  %c1 = alloca i8
  store i8 %.t1496, ptr %c1
  %.t1497 = load ptr, ptr %p2
  %.t1498 = load i8, ptr %.t1497
  %.t1499 = zext i8 %.t1498 to i64
  %.t1500 = trunc i64 %.t1499 to i8
  %c2 = alloca i8
  store i8 %.t1500, ptr %c2
  %.t1501 = load i8, ptr %c1
  %.t1502 = load i8, ptr %c2
  %.t1503 = icmp ne i8 %.t1501, %.t1502
  br i1 %.t1503, label %then1504, label %else1505
then1504:
  br label %merge1506
else1505:
  %.t1507 = load i8, ptr %c1
  %.t1508 = icmp eq i8 %.t1507, 0
  br i1 %.t1508, label %then1509, label %else1510
then1509:
  br label %merge1511
else1510:
  br label %merge1511
merge1511:
  %.t1512 = phi i1 [ 1, %then1509 ], [ 1, %else1510 ]
  br label %merge1506
merge1506:
  %.t1513 = phi i1 [ 0, %then1504 ], [ %.t1512, %merge1511 ]
  store i1 %.t1513, ptr %result
  %.t1514 = load i8, ptr %c1
  %.t1515 = load i8, ptr %c2
  %.t1516 = icmp ne i8 %.t1514, %.t1515
  br i1 %.t1516, label %then1517, label %else1518
then1517:
  br label %merge1519
else1518:
  %.t1520 = load i8, ptr %c1
  %.t1521 = icmp eq i8 %.t1520, 0
  br i1 %.t1521, label %then1522, label %else1523
then1522:
  br label %merge1524
else1523:
  br label %merge1524
merge1524:
  %.t1525 = phi i1 [ 0, %then1522 ], [ 1, %else1523 ]
  br label %merge1519
merge1519:
  %.t1526 = phi i1 [ 0, %then1517 ], [ %.t1525, %merge1524 ]
  store i1 %.t1526, ptr %continue
  %.t1527 = load ptr, ptr %p1
  %.t1528 = getelementptr i64, ptr %.t1527, i64 1
  store ptr %.t1528, ptr %p1
  %.t1529 = load ptr, ptr %p2
  %.t1530 = getelementptr i64, ptr %.t1529, i64 1
  store ptr %.t1530, ptr %p2
  br label %while_header1489
while_exit1491:
  %.t1531 = load i1, ptr %result
  ret i1 %.t1531
}

define i1 @str__starts_with(ptr %self, ptr %prefix) {
entry:
  %self.addr = alloca ptr
  store ptr %self, ptr %self.addr
  %prefix.addr = alloca ptr
  store ptr %prefix, ptr %prefix.addr
  %.t1532 = load ptr, ptr %self.addr
  %p_str = alloca ptr
  store ptr %.t1532, ptr %p_str
  %.t1533 = load ptr, ptr %prefix.addr
  %p_prefix = alloca ptr
  store ptr %.t1533, ptr %p_prefix
  %result = alloca i1
  store i1 1, ptr %result
  %continue = alloca i1
  store i1 1, ptr %continue
  br label %while_header1534
while_header1534:
  %.t1537 = load i1, ptr %continue
  br i1 %.t1537, label %while_body1535, label %while_exit1536
while_body1535:
  %.t1538 = load ptr, ptr %p_prefix
  %.t1539 = load i8, ptr %.t1538
  %.t1540 = zext i8 %.t1539 to i64
  %.t1541 = trunc i64 %.t1540 to i8
  %prefix_char = alloca i8
  store i8 %.t1541, ptr %prefix_char
  %.t1542 = load i8, ptr %prefix_char
  %.t1543 = icmp ne i8 %.t1542, 0
  br i1 %.t1543, label %then1544, label %else1545
then1544:
  br label %merge1546
else1545:
  br label %merge1546
merge1546:
  %.t1547 = phi i1 [ 1, %then1544 ], [ 0, %else1545 ]
  store i1 %.t1547, ptr %continue
  %.t1548 = load i1, ptr %continue
  br i1 %.t1548, label %then1549, label %else1550
then1549:
  %.t1552 = load ptr, ptr %p_str
  %.t1553 = load i8, ptr %.t1552
  %.t1554 = zext i8 %.t1553 to i64
  br label %merge1551
else1550:
  br label %merge1551
merge1551:
  %.t1555 = phi i64 [ %.t1554, %then1549 ], [ 0, %else1550 ]
  %.t1556 = trunc i64 %.t1555 to i8
  %c_str = alloca i8
  store i8 %.t1556, ptr %c_str
  %.t1557 = load i1, ptr %continue
  br i1 %.t1557, label %then1558, label %else1559
then1558:
  %.t1561 = load ptr, ptr %p_prefix
  %.t1562 = load i8, ptr %.t1561
  %.t1563 = zext i8 %.t1562 to i64
  br label %merge1560
else1559:
  br label %merge1560
merge1560:
  %.t1564 = phi i64 [ %.t1563, %then1558 ], [ 0, %else1559 ]
  %.t1565 = trunc i64 %.t1564 to i8
  %c_prefix = alloca i8
  store i8 %.t1565, ptr %c_prefix
  %.t1566 = load i1, ptr %continue
  br i1 %.t1566, label %then1567, label %else1568
then1567:
  %.t1570 = load i8, ptr %c_str
  %.t1571 = load i8, ptr %c_prefix
  %.t1572 = icmp ne i8 %.t1570, %.t1571
  br i1 %.t1572, label %then1573, label %else1574
then1573:
  br label %merge1575
else1574:
  br label %merge1575
merge1575:
  %.t1576 = phi i1 [ 0, %then1573 ], [ 1, %else1574 ]
  br label %merge1569
else1568:
  %.t1577 = load i1, ptr %result
  br label %merge1569
merge1569:
  %.t1578 = phi i1 [ %.t1576, %merge1575 ], [ %.t1577, %else1568 ]
  store i1 %.t1578, ptr %result
  %.t1579 = load i1, ptr %result
  br i1 %.t1579, label %then1580, label %else1581
then1580:
  %.t1583 = load i1, ptr %continue
  br label %merge1582
else1581:
  br label %merge1582
merge1582:
  %.t1584 = phi i1 [ %.t1583, %then1580 ], [ 0, %else1581 ]
  store i1 %.t1584, ptr %continue
  %.t1585 = load ptr, ptr %p_str
  %.t1586 = getelementptr i64, ptr %.t1585, i64 1
  store ptr %.t1586, ptr %p_str
  %.t1587 = load ptr, ptr %p_prefix
  %.t1588 = getelementptr i64, ptr %.t1587, i64 1
  store ptr %.t1588, ptr %p_prefix
  br label %while_header1534
while_exit1536:
  %.t1589 = load i1, ptr %result
  ret i1 %.t1589
}

define i1 @str__ends_with(ptr %self, ptr %suffix) {
entry:
  %self.addr = alloca ptr
  store ptr %self, ptr %self.addr
  %suffix.addr = alloca ptr
  store ptr %suffix, ptr %suffix.addr
  %.t1590 = load ptr, ptr %self.addr
  %.t1591 = call i64 @str__len(ptr %.t1590)
  %self_len = alloca i64
  store i64 %.t1591, ptr %self_len
  %.t1592 = load ptr, ptr %suffix.addr
  %.t1593 = call i64 @str__len(ptr %.t1592)
  %suffix_len = alloca i64
  store i64 %.t1593, ptr %suffix_len
  %.t1594 = load i64, ptr %suffix_len
  %.t1595 = load i64, ptr %self_len
  %.t1596 = icmp sgt i64 %.t1594, %.t1595
  br i1 %.t1596, label %then1597, label %else1598
then1597:
  br label %merge1599
else1598:
  br label %merge1599
merge1599:
  %.t1600 = phi i1 [ 0, %then1597 ], [ 1, %else1598 ]
  %result = alloca i1
  store i1 %.t1600, ptr %result
  %.t1601 = load i1, ptr %result
  br i1 %.t1601, label %then1602, label %else1603
then1602:
  %.t1605 = load i64, ptr %self_len
  %.t1606 = load i64, ptr %suffix_len
  %.t1607 = sub i64 %.t1605, %.t1606
  %start_pos = alloca i64
  store i64 %.t1607, ptr %start_pos
  %.t1608 = load ptr, ptr %self.addr
  %p_str = alloca ptr
  store ptr %.t1608, ptr %p_str
  %.t1609 = load ptr, ptr %p_str
  %.t1610 = load i64, ptr %start_pos
  %.t1611 = getelementptr i64, ptr %.t1609, i64 %.t1610
  store ptr %.t1611, ptr %p_str
  %.t1612 = load ptr, ptr %suffix.addr
  %p_suffix = alloca ptr
  store ptr %.t1612, ptr %p_suffix
  %continue = alloca i1
  store i1 1, ptr %continue
  %is_match = alloca i1
  store i1 1, ptr %is_match
  br label %while_header1613
while_header1613:
  %.t1616 = load i1, ptr %continue
  br i1 %.t1616, label %while_body1614, label %while_exit1615
while_body1614:
  %.t1617 = load ptr, ptr %p_suffix
  %.t1618 = load i8, ptr %.t1617
  %.t1619 = zext i8 %.t1618 to i64
  %.t1620 = trunc i64 %.t1619 to i8
  %suffix_char = alloca i8
  store i8 %.t1620, ptr %suffix_char
  %.t1621 = load i8, ptr %suffix_char
  %.t1622 = icmp ne i8 %.t1621, 0
  br i1 %.t1622, label %then1623, label %else1624
then1623:
  br label %merge1625
else1624:
  br label %merge1625
merge1625:
  %.t1626 = phi i1 [ 1, %then1623 ], [ 0, %else1624 ]
  store i1 %.t1626, ptr %continue
  %.t1627 = load i1, ptr %continue
  br i1 %.t1627, label %then1628, label %else1629
then1628:
  %.t1631 = load ptr, ptr %p_str
  %.t1632 = load i8, ptr %.t1631
  %.t1633 = zext i8 %.t1632 to i64
  br label %merge1630
else1629:
  br label %merge1630
merge1630:
  %.t1634 = phi i64 [ %.t1633, %then1628 ], [ 0, %else1629 ]
  %.t1635 = trunc i64 %.t1634 to i8
  %c_str = alloca i8
  store i8 %.t1635, ptr %c_str
  %.t1636 = load i1, ptr %continue
  br i1 %.t1636, label %then1637, label %else1638
then1637:
  %.t1640 = load ptr, ptr %p_suffix
  %.t1641 = load i8, ptr %.t1640
  %.t1642 = zext i8 %.t1641 to i64
  br label %merge1639
else1638:
  br label %merge1639
merge1639:
  %.t1643 = phi i64 [ %.t1642, %then1637 ], [ 0, %else1638 ]
  %.t1644 = trunc i64 %.t1643 to i8
  %c_suffix = alloca i8
  store i8 %.t1644, ptr %c_suffix
  %.t1645 = load i1, ptr %continue
  br i1 %.t1645, label %then1646, label %else1647
then1646:
  %.t1649 = load i8, ptr %c_str
  %.t1650 = load i8, ptr %c_suffix
  %.t1651 = icmp ne i8 %.t1649, %.t1650
  br i1 %.t1651, label %then1652, label %else1653
then1652:
  br label %merge1654
else1653:
  br label %merge1654
merge1654:
  %.t1655 = phi i1 [ 0, %then1652 ], [ 1, %else1653 ]
  br label %merge1648
else1647:
  %.t1656 = load i1, ptr %is_match
  br label %merge1648
merge1648:
  %.t1657 = phi i1 [ %.t1655, %merge1654 ], [ %.t1656, %else1647 ]
  store i1 %.t1657, ptr %is_match
  %.t1658 = load i1, ptr %is_match
  br i1 %.t1658, label %then1659, label %else1660
then1659:
  %.t1662 = load i1, ptr %continue
  br label %merge1661
else1660:
  br label %merge1661
merge1661:
  %.t1663 = phi i1 [ %.t1662, %then1659 ], [ 0, %else1660 ]
  store i1 %.t1663, ptr %continue
  %.t1664 = load ptr, ptr %p_str
  %.t1665 = getelementptr i64, ptr %.t1664, i64 1
  store ptr %.t1665, ptr %p_str
  %.t1666 = load ptr, ptr %p_suffix
  %.t1667 = getelementptr i64, ptr %.t1666, i64 1
  store ptr %.t1667, ptr %p_suffix
  br label %while_header1613
while_exit1615:
  %.t1668 = load i1, ptr %is_match
  br label %merge1604
else1603:
  br label %merge1604
merge1604:
  %.t1669 = phi i1 [ %.t1668, %while_exit1615 ], [ 0, %else1603 ]
  store i1 %.t1669, ptr %result
  %.t1670 = load i1, ptr %result
  ret i1 %.t1670
}

define i64 @str__find(ptr %self, ptr %needle) {
entry:
  %self.addr = alloca ptr
  store ptr %self, ptr %self.addr
  %needle.addr = alloca ptr
  store ptr %needle, ptr %needle.addr
  %.t1671 = load ptr, ptr %self.addr
  %p_haystack = alloca ptr
  store ptr %.t1671, ptr %p_haystack
  %.t1672 = sext i32 0 to i64
  %pos = alloca i64
  store i64 %.t1672, ptr %pos
  %found = alloca i1
  store i1 0, ptr %found
  %.t1673 = sub i32 0, 1
  %.t1674 = sext i32 %.t1673 to i64
  %result = alloca i64
  store i64 %.t1674, ptr %result
  %continue = alloca i1
  store i1 1, ptr %continue
  %.t1675 = load ptr, ptr %self.addr
  %p_check = alloca ptr
  store ptr %.t1675, ptr %p_check
  %.t1676 = load ptr, ptr %needle.addr
  %p_needle = alloca ptr
  store ptr %.t1676, ptr %p_needle
  %matches = alloca i1
  store i1 1, ptr %matches
  %inner_continue = alloca i1
  store i1 1, ptr %inner_continue
  br label %while_header1677
while_header1677:
  %.t1680 = load i1, ptr %continue
  br i1 %.t1680, label %while_body1678, label %while_exit1679
while_body1678:
  %.t1681 = load ptr, ptr %p_haystack
  %.t1682 = load i8, ptr %.t1681
  %.t1683 = zext i8 %.t1682 to i64
  %.t1684 = trunc i64 %.t1683 to i8
  %haystack_char = alloca i8
  store i8 %.t1684, ptr %haystack_char
  %.t1685 = load i8, ptr %haystack_char
  %.t1686 = icmp ne i8 %.t1685, 0
  br i1 %.t1686, label %then1687, label %else1688
then1687:
  br label %merge1689
else1688:
  br label %merge1689
merge1689:
  %.t1690 = phi i1 [ 1, %then1687 ], [ 0, %else1688 ]
  store i1 %.t1690, ptr %continue
  %.t1691 = load i1, ptr %continue
  br i1 %.t1691, label %then1692, label %else1693
then1692:
  %.t1695 = load ptr, ptr %p_haystack
  br label %merge1694
else1693:
  %.t1696 = load ptr, ptr %p_check
  br label %merge1694
merge1694:
  %.t1697 = phi ptr [ %.t1695, %then1692 ], [ %.t1696, %else1693 ]
  store ptr %.t1697, ptr %p_check
  %.t1698 = load i1, ptr %continue
  br i1 %.t1698, label %then1699, label %else1700
then1699:
  %.t1702 = load ptr, ptr %needle.addr
  br label %merge1701
else1700:
  %.t1703 = load ptr, ptr %p_needle
  br label %merge1701
merge1701:
  %.t1704 = phi ptr [ %.t1702, %then1699 ], [ %.t1703, %else1700 ]
  store ptr %.t1704, ptr %p_needle
  %.t1705 = load i1, ptr %continue
  br i1 %.t1705, label %then1706, label %else1707
then1706:
  br label %merge1708
else1707:
  %.t1709 = load i1, ptr %matches
  br label %merge1708
merge1708:
  %.t1710 = phi i1 [ 1, %then1706 ], [ %.t1709, %else1707 ]
  store i1 %.t1710, ptr %matches
  %.t1711 = load i1, ptr %continue
  br i1 %.t1711, label %then1712, label %else1713
then1712:
  br label %merge1714
else1713:
  %.t1715 = load i1, ptr %inner_continue
  br label %merge1714
merge1714:
  %.t1716 = phi i1 [ 1, %then1712 ], [ %.t1715, %else1713 ]
  store i1 %.t1716, ptr %inner_continue
  br label %while_header1717
while_header1717:
  %.t1720 = load i1, ptr %inner_continue
  br i1 %.t1720, label %while_body1718, label %while_exit1719
while_body1718:
  %.t1721 = load ptr, ptr %p_needle
  %.t1722 = load i8, ptr %.t1721
  %.t1723 = zext i8 %.t1722 to i64
  %.t1724 = trunc i64 %.t1723 to i8
  %needle_char = alloca i8
  store i8 %.t1724, ptr %needle_char
  %.t1725 = load i8, ptr %needle_char
  %.t1726 = icmp ne i8 %.t1725, 0
  br i1 %.t1726, label %then1727, label %else1728
then1727:
  br label %merge1729
else1728:
  br label %merge1729
merge1729:
  %.t1730 = phi i1 [ 1, %then1727 ], [ 0, %else1728 ]
  store i1 %.t1730, ptr %inner_continue
  %.t1731 = load i1, ptr %inner_continue
  br i1 %.t1731, label %then1732, label %else1733
then1732:
  %.t1735 = load ptr, ptr %p_check
  %.t1736 = load i8, ptr %.t1735
  %.t1737 = zext i8 %.t1736 to i64
  br label %merge1734
else1733:
  br label %merge1734
merge1734:
  %.t1738 = phi i64 [ %.t1737, %then1732 ], [ 0, %else1733 ]
  %.t1739 = trunc i64 %.t1738 to i8
  %c_check = alloca i8
  store i8 %.t1739, ptr %c_check
  %.t1740 = load i1, ptr %inner_continue
  br i1 %.t1740, label %then1741, label %else1742
then1741:
  %.t1744 = load ptr, ptr %p_needle
  %.t1745 = load i8, ptr %.t1744
  %.t1746 = zext i8 %.t1745 to i64
  br label %merge1743
else1742:
  br label %merge1743
merge1743:
  %.t1747 = phi i64 [ %.t1746, %then1741 ], [ 0, %else1742 ]
  %.t1748 = trunc i64 %.t1747 to i8
  %c_needle = alloca i8
  store i8 %.t1748, ptr %c_needle
  %.t1749 = load i1, ptr %inner_continue
  br i1 %.t1749, label %then1750, label %else1751
then1750:
  %.t1753 = load i8, ptr %c_check
  %.t1754 = load i8, ptr %c_needle
  %.t1755 = icmp ne i8 %.t1753, %.t1754
  br i1 %.t1755, label %then1756, label %else1757
then1756:
  br label %merge1758
else1757:
  %.t1759 = load i1, ptr %matches
  br label %merge1758
merge1758:
  %.t1760 = phi i1 [ 0, %then1756 ], [ %.t1759, %else1757 ]
  br label %merge1752
else1751:
  %.t1761 = load i1, ptr %matches
  br label %merge1752
merge1752:
  %.t1762 = phi i1 [ %.t1760, %merge1758 ], [ %.t1761, %else1751 ]
  store i1 %.t1762, ptr %matches
  %.t1763 = load ptr, ptr %p_check
  %.t1764 = getelementptr i64, ptr %.t1763, i64 1
  store ptr %.t1764, ptr %p_check
  %.t1765 = load ptr, ptr %p_needle
  %.t1766 = getelementptr i64, ptr %.t1765, i64 1
  store ptr %.t1766, ptr %p_needle
  br label %while_header1717
while_exit1719:
  %.t1767 = load i1, ptr %continue
  br i1 %.t1767, label %then1768, label %else1769
then1768:
  %.t1771 = load i1, ptr %matches
  br label %merge1770
else1769:
  %.t1772 = load i1, ptr %found
  br label %merge1770
merge1770:
  %.t1773 = phi i1 [ %.t1771, %then1768 ], [ %.t1772, %else1769 ]
  store i1 %.t1773, ptr %found
  %.t1774 = load i1, ptr %found
  br i1 %.t1774, label %then1775, label %else1776
then1775:
  %.t1778 = load i64, ptr %pos
  br label %merge1777
else1776:
  %.t1779 = load i64, ptr %result
  br label %merge1777
merge1777:
  %.t1780 = phi i64 [ %.t1778, %then1775 ], [ %.t1779, %else1776 ]
  store i64 %.t1780, ptr %result
  %.t1781 = load i1, ptr %found
  br i1 %.t1781, label %then1782, label %else1783
then1782:
  br label %merge1784
else1783:
  %.t1785 = load i1, ptr %continue
  br label %merge1784
merge1784:
  %.t1786 = phi i1 [ 0, %then1782 ], [ %.t1785, %else1783 ]
  store i1 %.t1786, ptr %continue
  %.t1787 = load ptr, ptr %p_haystack
  %.t1788 = getelementptr i64, ptr %.t1787, i64 1
  store ptr %.t1788, ptr %p_haystack
  %.t1789 = load i64, ptr %pos
  %.t1790 = add i64 %.t1789, 1
  store i64 %.t1790, ptr %pos
  br label %while_header1677
while_exit1679:
  %.t1791 = load i64, ptr %result
  ret i64 %.t1791
}

define i1 @str__contains(ptr %self, ptr %needle) {
entry:
  %self.addr = alloca ptr
  store ptr %self, ptr %self.addr
  %needle.addr = alloca ptr
  store ptr %needle, ptr %needle.addr
  %.t1792 = load ptr, ptr %self.addr
  %.t1793 = load ptr, ptr %needle.addr
  %.t1794 = call i64 @str__find(ptr %.t1792, ptr %.t1793)
  %pos = alloca i64
  store i64 %.t1794, ptr %pos
  %.t1795 = load i64, ptr %pos
  %.t1796 = icmp sge i64 %.t1795, 0
  ret i1 %.t1796
}

define i8 @str__char_at(ptr %self, i64 %index) {
entry:
  %self.addr = alloca ptr
  store ptr %self, ptr %self.addr
  %index.addr = alloca i64
  store i64 %index, ptr %index.addr
  %.t1797 = load ptr, ptr %self.addr
  %.t1798 = load i64, ptr %index.addr
  %.t1799 = call i8 @str__at(ptr %.t1797, i64 %.t1798)
  ret i8 %.t1799
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
