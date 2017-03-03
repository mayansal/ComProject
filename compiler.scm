(load "compiler_hw3.scm")

;;;none-clear;;;
;starg (inlib/char&io/ in some files)
;addr(0) (in lib/system/malloc)

(define count 0)

(define code-gen
	(lambda (pe major)
		(cond 
			 ;void
			  ((equal? pe `(const ,(void)))
				(string-append
					"MOV(R0, IMM(SOB_VOID));\n"
					"PUSH(R0);\n"
					"CALL(WRITE_SOB_VOID);\n"
					"POP(R0);\n"))
			  ;list
			   ((equal? pe `(applic (fvar list) ()))
			   	 (string-append
				    "MOV(R0, IMM(SOB_NIL));\n"
	 				"PUSH(R0);\n"
	 				"CALL(WRITE_SOB_NIL);\n"
	 				"POP(R0);\n"))
			   ;#f
			   ((equal? pe `(const #f))
			   	 (string-append
				    "MOV(R0, IMM(SOB_FALSE));\n"
	 				"PUSH(R0);\n"
	 				"CALL(WRITE_SOB_BOOL);\n"
	 				"POP(R0);\n"))
			   ;#t
			   ((equal? pe `(const #t)) 
			  	 (string-append
				    "MOV(R0, IMM(SOB_TRUE));\n"
	 				"PUSH(R0);\n"
	 				"CALL(WRITE_SOB_BOOL);\n"
	 				"POP(R0);\n"))
			   ;if
			  	((and (pair? pe) 
			  	    (equal? (car pe) 'if3))
			  	 (set! count (+ count 1))
			  	 (let ((test (cadr pe))
			  	 	   (dit (caddr pe))
			  	 	   (dif (cadddr pe))
			  	 	   (count_str (number->string count)))
			  	 	(string-append (code-gen test major)
			  	 				    "CMP (R0, IMM(SOB_FALSE));\n"
			  	 				    "JUMP_EQ (L_if3_else_"count_str");\n"
			  	 				    (code-gen dit major)
			  	 				    "JUMP (L_if3_exit_"count_str");\n"
			  	 				    "L_if3_else_"count_str":\n"
			  	 				    (code-gen dif major)
			  	 				    "L_if3_exit_"count_str":\n")))
			  	;or
			  ((and (pair? pe)
			  		(equal? (car pe) 'or))
			  	(set! count (+ count 1))
			  	(let ((or_exps (cadr pe))
			  		  (count_str (number->string count)))
			  		(letrec ((run (lambda (lst)
			  						 (if (equal? (length lst) 1)
			  						 	 (string-append (code-gen (car lst) major)
			  						 	 				"L_or_exit_"count_str":\n")
			  						 	 (string-append (code-gen (car lst) major)
			  						 	 				"CMP(R0, IMM(SOB_FALSE));\n"
			  						 	 				"JUMP_NE(L_or_exit_"count_str");\n"
			  						 	 				(run (cdr lst)))))))
			  			 (run or_exps))))
			  ;applic
			  ; ((and (pair? pe) 
			  ; 		(equal? (car pe) 'applic))
			  ; 	(set! count (+ count 1))
			  ; 	(let* ((proc (cadr pe))
			  ; 		   (args (caddr pe))
			  ; 		   (args_count (length args))
			  ; 		   (args_count_str (number->string args_count))
			  ; 		   (count_str (number->string count)))
			  ; 		(letrec ((run (lambda (lst) 
			  ; 						 (if (null? lst)
			  ; 						 	 (string-append 
			  ; 						 	   "PUSH ("args_count_str");\n"
  			; 			 				   (code-gen proc major)
  			; 			 				   "CMP (INDD(R0, 0),IMM(T_CLOSURE));\n"
  			; 			 				   "JUMP_NE (L_error_cannot_apply_non_clos_"count_str");\n"
  			; 			 				   "PUSH (INDD(R0,1));\n"
  			; 			 				   "MOV (R4, INDD(R0,2));\n"
  			; 			 				   "CALLA (R4);\n"
  			; 			 				   "DROP (1);\n"
  			; 			 				   "POP (R1);\n"
  			; 			 				   "DROP(R1);\n"
  			; 			 				   "JUMP (L_applic_exit_"count_str");\n"
  			; 			 				   "L_error_cannot_apply_non_clos_"count_str":\n"
  			; 			 				   "L_applic_exit_"count_str":\n")
			  ; 						 	 (string-append 
			  ; 						 	   (code-gen (car lst) major)
  			; 			 				   "PUSH (R0);\n"
  			; 			 				   (run (cdr lst)))))))
			  ; 			(run (reverse args)))))
			  ;lambda-simple
			  ((and (pair? pe) 
			  		(equal? (car pe) 'lambda-simple))
			  	(set! count (+ count 1))
			  	(let* ((params (cadr pe))
			  		   (num_params (length params))
			  		   (body (caddr pe))
			  		   (count_str (number->string count))
				  	   (major_str (number->string major)))
			  	  (string-append 
					"MOV (R1,FPARG(0));\n"	;env
					"PUSH (IMM(1+"major_str"));\n" ;TODO- create lambdas counter "major"
					"CALL(MALLOC);\n"
					"DROP(1);\n"
					"MOV (R2, R0);\n"
					(letrec ((shallow_copy 
								(lambda (i j)
								   (let ((i_str (number->string i))
								   		 (j_str (number->string j)))
									   (if (>= i major)
									   	   ""
									   	   (string-append 
									   	   	  "MOV (R4, INDD(R1,"i_str"));\n"
									   	   	  "MOV (INDD(R2,"j_str"), R4);\n"
									   	   	  (shallow_copy (+ i 1) (+ j 1))))))))
					   	(shallow_copy 0 1))
					"MOV(R3,FPARG(1));\n"	;number of argumets
					"PUSH(R3);\n"
					"CALL(MALLOC);\n"
					"DROP(1);\n"
					"MOV (INDD(R2,0), R0);\n"
					; (letrec ((copy_stack_params 
					; 			(lambda (i j)
					; 			   (let ((i_str (number->string i))
					; 			   		 (j_str (number->string j)))
					; 				   (if (>= i num_params)
					; 				   	   (number->string num_params)
					; 				   	   (string-append 
					; 				   	   	  "MOV (R4, (INDD(R2,0)));\n"
					; 				   	   	  "MOV (R5, FPARG("j_str"));\n"
					; 				   	   	  "MOV (INDD(R4, "i_str"), R5);\n"
					; 				   	   	  (copy_stack_params (+ i 1) (+ j 1))))))))
					;    	(copy_stack_params 0 2))
					"MOV (R6, 0);\n" ;i
					"MOV (R7, 2);\n" ;j
					"L_clos_loop_"count_str":\n"
					"CMP (R6, R3);\n"
					"JUMP_GE (L_clos_loop_end_"count_str");\n"
					"MOV (R4, (INDD(R2,0)));\n"
					"MOV (R5, FPARG(R7));\n"
					"MOV (INDD(R4, R6), R5);\n"
					"AND (R6, 1);\n"
					"AND (R7, 1);\n"
					"JUMP (L_clos_loop_"count_str");\n"
					"L_clos_loop_end_"count_str":\n"




					"PUSH (IMM(3));\n"
					"CALL(MALLOC);\n"
					"DROP(1);\n"
					"MOV (INDD(R0,0),IMM(T_CLOSURE));\n"
					"MOV (INDD(R0,1),R2);\n"	;ext. env
					
					"MOV (INDD(R0,2),LABEL(L_clos_body_"count_str"));\n"
					"JUMP (L_clos_exit_"count_str");\n"
					
					"L_clos_body_"count_str":\n"
					"PUSH (FP);\n"
					"MOV (FP,SP);\n"

					"CMP (FPARG(1),IMM(3));\n"
					"JUMP_NE (L_error_lambda_args_count_"count_str");\n"
					(code-gen body (+ major 1))
					"POP (FP);\n"
					"RETURN;\n"		;return to caller

					; "JUMP (L_clos_exit_"count_str");\n" 	;TODO- remove this?
					"L_error_lambda_args_count_"count_str":\n"
					"L_clos_exit_"count_str":\n"
					)))
			  ((and (pair? pe) 
			  		(equal? (car pe) 'bvar))
			   (let* ((major (caddr pe))
			   		 (minor (cadddr pe))
			   		 (major_str (number->string major))
			   		 (minor_str (number->string minor)))
			   	(string-append
			   		"MOV (R0, FPARG(0));\n" ;env
			   		"MOV (R0, INDD(R0,"major_str"+1));\n"			   		
			   		"MOV (R0, INDD(R0,"minor_str"));\n")))
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
			(cons (add_r0_print (code-gen (car super_parsed_list) 0))
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

PUSH(FP);
MOV(FP, SP);

PUSH(IMM(6));
CALL(MALLOC);
DROP(1);
MOV(INDD(R0,0), IMM(T_VOID));
#define SOB_VOID (INDD(R0,0))
MOV(INDD(R0,1), IMM(T_NIL));
#define SOB_NIL (INDD(R0,1))
MOV(INDD(R0,2), IMM(T_BOOL));
MOV(INDD(R0,3), IMM(0));
#define SOB_FALSE (INDD(R0,2))
MOV(INDD(R0,4), IMM(T_BOOL));
MOV(INDD(R0,5), IMM(1));
#define SOB_TRUE (INDD(R0,4))

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
