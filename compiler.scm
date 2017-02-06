(load "compiler_hw3.scm")

;;;none-clear;;;
;starg (inlib/char&io/ in some files)
;addr(0) (in lib/system/malloc)

(define count 0)

(define code-gen
	(lambda (pe)
		(cond ((equal? pe `(const ,(void)))
				(string-append
					"MOV(R0, IMM(SOB_VOID));\n"
					"PUSH(R0);\n"
					"CALL(WRITE_SOB_VOID);\n"
					"POP(R0);\n"))
			   ((equal? pe `(applic (fvar list) ()))
			   	 (string-append
				    "MOV(R0, IMM(SOB_NIL));\n"
	 				"PUSH(R0);\n"
	 				"CALL(WRITE_SOB_NIL);\n"
	 				"POP(R0);\n"))
			   ((equal? pe `(const #f))
			   	 (string-append
				    "MOV(R0, IMM(SOB_FALSE));\n"
	 				"PUSH(R0);\n"
	 				"CALL(WRITE_SOB_BOOL);\n"
	 				"POP(R0);\n"))
			   ((equal? pe `(const #t)) 
			  	 (string-append
				    "MOV(R0, IMM(SOB_TRUE));\n"
	 				"PUSH(R0);\n"
	 				"CALL(WRITE_SOB_BOOL);\n"
	 				"POP(R0);\n"))
			  ((and (pair? pe) 
			  	    (equal? (car pe) 'if3))
			  	 (set! count (+ count 1))
			  	 (let ((test (cadr pe))
			  	 	   (dit (caddr pe))
			  	 	   (dif (cadddr pe))
			  	 	   (count_str (number->string count)))
			  	 	(string-append (code-gen test)
			  	 				    "CMP (R0, IMM(SOB_FALSE));\n"
			  	 				    "JUMP_EQ (L_if3_else_"count_str");\n"
			  	 				    (code-gen dit)
			  	 				    "JUMP (L_if3_exit_"count_str");\n"
			  	 				    "L_if3_else_"count_str":\n"
			  	 				    (code-gen dif)
			  	 				    "L_if3_exit_"count_str":\n")))
			  ((and (pair? pe)
			  		(equal? (car pe) 'or))
			  	(set! count (+ count 1))
			  	(let ((or_exps (cadr pe))
			  		  (count_str (number->string count)))
			  		(letrec ((run (lambda (lst)
			  						 (if (equal? (length lst) 1)
			  						 	 (string-append (code-gen (car lst))
			  						 	 				"L_or_exit_"count_str":\n")
			  						 	 (string-append (code-gen (car lst))
			  						 	 				"CMP(R0, IMM(SOB_FALSE));\n"
			  						 	 				"JUMP_NE(L_or_exit_"count_str");\n"
			  						 	 				(run (cdr lst)))))))
			  			 (run or_exps))))
			  
			  (else ""))))

(define compile-scheme-file
	(lambda (scm_src_file asm_target_file)
		(let* ((scm_content (file->string scm_src_file))
			   (match_and_remain (test-string <Sexpr> scm_content))
			   (sexprs_list (create_sexprs_list scm_content))
			   (super_parsed_list (parsed_and_hw3 sexprs_list))
			   (constant_table (build_constant_table super_parsed_list))
			   (global_var_table (build_global_var_table super_parsed_list))
			   (asm_instructions_list (build_asm_insts_list super_parsed_list))
			   (asm_instructions_string (build_asm_insts_string asm_instructions_list))
			   (final_asm (add_prologue_epilgue asm_instructions_string)))
			(string->file final_asm asm_target_file))))
;super_parsed_list)))

;TODO - ONLY ONE S-EXP
(define build_asm_insts_list
	(lambda (super_parsed_list)
		(if (null? super_parsed_list)
			(list)
			(cons (add_r0_print (code-gen (car super_parsed_list)))
				  (build_asm_insts_list (cdr super_parsed_list))))))

;TODO - PROBABLY REMOVE
(define add_r0_print
	(lambda (asm_string)
		;(string-append asm_string "OUT(IMM(2), R0);\n ")
		asm_string))

(define build_asm_insts_string
	(lambda (insts_list)
		(if (null? insts_list)
			""
			(string-append (car insts_list) (build_asm_insts_string (cdr insts_list))))))


; /* change to 0 for no debug info to be printed: */
; #define DO_SHOW 1

(define add_prologue_epilgue
	(lambda (asm_insts_string)
		(string-append "
#include <stdio.h>
#include <stdlib.h>

#include \"cisc.h\"

/* change to 0 for no debug info to be printed: */
#define DO_SHOW 1

#include \"debug_macros.h\"

int main()
{
START_MACHINE;

JUMP(CONTINUE);

#include \"char.lib\"
#include \"io.lib\"
#include \"math.lib\"
#include \"string.lib\"
#include \"system.lib\"
#include \"scheme.lib\"

CONTINUE:

/*TODO - should entered the constant_table*/
MOV (IND(0), IMM(T_VOID))
#define SOB_VOID 0
MOV (IND(1), IMM(T_NIL))
#define SOB_NIL 1
MOV (IND(2), IMM(T_BOOL))
MOV (IND(3), IMM(0))
#define SOB_FALSE 2
MOV (IND(4), IMM(T_BOOL))
MOV (IND(5), IMM(1))
#define SOB_TRUE 4



PUSH(FP);
MOV(FP, SP);

"
 asm_insts_string
"
POP(FP);

/*TODO - remove info - for debug*/
INFO;

STOP_MACHINE;

return 0;
}"
						)))



;TODO
(define build_constant_table
	(lambda (super_parsed_list)
		(list)))

;TODO
(define build_global_var_table
	(lambda (super_parsed_list)
		(list)))

(define parsed_and_hw3 
	(lambda (sexprs_list)
		(if (null? sexprs_list)
			(list)
			(cons (annotate-tc
				   	 (pe->lex-pe
				   	   (box-set
				   	      (remove-applic-lambda-nil
				   	      	(eliminate-nested-defines 
				   	      		(parse (car sexprs_list)))))))
				   (parsed_and_hw3 (cdr sexprs_list))))))

(define create_sexprs_list
	(lambda (scm_content)
		(if (equal? scm_content "")
			(list)
			(let* ((match_and_remain (test-string <Sexpr> scm_content))
				   (match (cadar match_and_remain))
				   (remain (cadadr match_and_remain)))
				(cons match (create_sexprs_list remain))))))



(define file->string
	(lambda (in-file)
		(let ((in-port (open-input-file in-file)))
			(letrec ((run
						(lambda ()
							(let ((ch (read-char in-port)))
								(if (eof-object? ch)
									(begin
										(close-input-port in-port)
										'())
									(cons ch (run)))))))

				(list->string (run))))))


(define string->file
	(lambda (string out-file)
		(let ((out-port (open-output-file out-file)))
			(begin (display string out-port)
				   (close-output-port out-port)))))
