
;from bottom (old):

; (define remww
; 	(lambda (lst)
; 		(if (null? lst)
; 			lst
; 			(remww_helper (reverse lst)))))

; (define remww_helper
; 	(lambda (lst)
; 		(if (null? lst)
; 			lst)
; 			(let* ((curr_inst (car lst))
; 				   (write_registers (caddr curr_inst)))
; 				(if (null? write_registers)
; 					(remww_helper (cdr lst))
; 					(let ((curr_remww_lst (do_remww lst write_registers)))
; 						(cons (car curr_remww_lst)
; 							  (remww_helper (cdr curr_remww_lst))))))))

; ; checks if any instruction, preceding the current, can be removed-ww
; (define do_remww
; 	(lambda (inst_lst  write_registers)
; 		(let (())))




;from top (new):

(define remww
	(lambda (lst)
		(if (null? lst)
			lst
			(remww_fixed_point lst))))

(define remww_fixed_point
	(lambda (lst)
		(let ((new_lst (remww_helper lst)))
			(if (equal? lst new_lst)
				lst
				(remww_fixed_point new_lst)))))

(define remww_helper
	(lambda (lst)
		(if (null? lst)
			lst
			(let* ((curr_inst (car lst))
				   (write_registers (caddr curr_inst))
				   (rest_lst (cdr lst)))
				(cond ((null? write_registers)
							(remww_helper rest_lst))
					  ((null? rest_lst)
							(list curr_inst))
					  ((should_remww? rest_lst write_registers)
							(remww_helper rest_lst))
					  (else 
					  		`(,curr_inst ,@(remww_helper rest_lst))))))))
;TODO
; (cond (null? write_registers))

; checks if any instruction, preceding the current, can be removed-ww
; and returns lst without the redundant instructions.
(define should_remww?
	(lambda (rest_lst write_registers)
		(if (null? write_registers)
			#t
			(let ((register (car write_registers)))
				(if (is_register_needed? register rest_lst)
					#f
					(should_remww? rest_lst (cdr write_registers)))))))

(define is_register_needed?
	(lambda (register rest_lst)
		(if (null? rest_lst)
			#t
			(let* ((curr_inst (car rest_lst))
				   (curr_read (cadr curr_inst))
				   (curr_write (caddr curr_inst)))
				(cond ((member register curr_read)
							#t)
					  ((member register curr_write)
							#f)
					  (else 
					  		(is_register_needed? register (cdr rest_lst))))))))
