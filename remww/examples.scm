
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; remww examples  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; from the project:
((inst1 () (1))
 (inst2 () (1)))

((inst1 (1) (1))
 (isnt2 (1) (1))
 (inst3 () (1)))

((g46494 (6 1) (6))
 (g46495 (3 6 0) (0 4))
 (g46496 (1) (3))
 (g46497 (5 0 6) (4 2))
 (g46498 (7 1 3) ())
 (g46499 (7) ())
 (g46500 (0 1) ())
 (g46501 (4 7) (7))
 (g46502 (5 7 0) ())
 (g46503 (5 2 0) (6 2))
 (g46504 (6 4 5) (0))
 (g46505 (1 2) (1))
 (g46506 (6 2 7 5) (7))
 (g46507 (1) ())
 (g46508 (1) ())
 (g46509 (0 3 2) (5 0))
 (g46510 (3 6) (4)))
; res:   ((g46494 (6 1) (6)) (g46495 (3 6 0) (0 4)) (g46496 (1) (3))
; 		(g46497 (5 0 6) (4 2)) (g46501 (4 7) (7))
; 		(g46503 (5 2 0) (6 2)) (g46504 (6 4 5) (0))
; 		(g46505 (1 2) (1)) (g46506 (6 2 7 5) (7))
; 		(g46509 (0 3 2) (5 0)) (g46510 (3 6) (4)))

((g46494 (6 1) (4))
 (g46495 (3 6 0) (0 4))
 (g46496 (1) (4))
 (g46497 (5 4 6) (4 2))
 (g46498 (7 1 3) ()))
;;; 5 6 7 will stay

((g46494 () (1))
 (g46495 () (2))
 (g46496 () (3))
 (g46497 (5) (4))
 (g46498 (7) (1 2 3 4)))
;;; 8 will stay


; ours:
;;;;;;;;

(remww '
((g46496 (5) ())
	)
)

;;;;;
(define e7 
  	'((inst1 (2) (2))
      (isnt2 (2) (2))
      (isnt3 (2) (2))
      (isnt4 (2) (2))
      (inst5 () (2))

      (inst1 (1) (1))
      (isnt2 (1) (1))
      (isnt3 (1) (1))
      (isnt4 (1) (1))
      (inst5 () (1))))

(remww e7)

; should return: 
; ((inst5 () (1)))
> (remww e7)
((inst1 (1) (1)) (isnt2 (1) (1)) (isnt3 (1) (1)) (inst5 () (1)))
> (remww (remww e7))
((inst1 (1) (1)) (isnt2 (1) (1)) (inst5 () (1)))
> (remww (remww (remww e7)))
((inst1 (1) (1)) (inst5 () (1)))
> (remww (remww (remww (remww e7))))
((inst5 () (1)))
> (remww (remww (remww (remww (remww e7)))))
((inst5 () (1)))
> (remww (remww (remww (remww (remww (remww e7))))))
((inst5 () (1)))
;;;;;

(remww e7)

(remww (remww e7))

(remww (remww (remww e7)))

(remww (remww (remww (remww e7))))

(remww (remww (remww (remww (remww e7)))))

(remww (remww (remww (remww (remww (remww e7))))))




; test 1 ;
()
; should return: 
; ()

((g1 () (1 4)))
; should return: 
; ((g1 () (1 4)))

((g1 () ()))
; should return: 
; ()

((g1 (1 2) ()))
; should return: 
; ()

((g1 (1 2) (1 2)))
; should return: 
; ((g1 (1 2) (1 2)))

((g1 (1 2 3 4) (1 2)))
; should return: 
; the same

((g1 (1 2 3 4) (1 2 3 4 5)))
; should return: 
; the same

((g1 () (1 2 3 4 5 6 7 8 9)))
; should return: 
; the same


(remww '
((g1 ()    (1))
 (g2 (1 4) (1))
 (g3 (1)   (1))
 (g4 ()    (6 7)))
	)


((g1 ()    (1 4))
 (g2 (1 4) (1 6))
 (g3 (1)   (1 4)))
; should return: 
; the same

((g1 ()    (1 4))
 (g2 (1)   (1 6))
 (g3 (1)   (1 4))
 (g4 ()    (6 7)))
; should return: 
; the same

((g1 ()    (1))
 (g2 (1 4) (1))
 (g3 (1)   (1))
 (g4 ()    (6 7)))
; should return: 
; the same

((g1 ()    (1))
 (g2 (1 4) (1))
 (g3 ()    (1))
 (g4 ()    (6 7)))
; should return: 
;  (g3 ()  (1))
;  (g4 ()  (6 7)))


((g1 ()    (1))
 (g2 (1 4) (1))
 (g3 ()    (1))
 (g4 ()    (6 7))
 (g5 ()    (6 5)))
; should return: 
; 

((g1 ()    (1 4))
 (g2 ()    (1 6))
 (g3 (1)   (1 4))
 (g4 ()    (6 7)))
; should return: 
; the same

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define remww
	(lambda (lst)
		(if (null? lst)
			lst
			(remww_helper lst))))

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
							curr_inst)
					  ((should_remww? rest_lst write_registers)
							(remww_helper rest_lst))
					  (else 
					  		(cons curr_inst
								 (remww_helper rest_lst))))))))
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
					(should_remww rest_lst (cdr write_registers)))))))


(define is_register_needed?
	(lambda (register rest_lst)
		(let* ((curr_inst (car rest_lst))
			   (curr_read (cadr curr_inst))
			   (curr_write (caddr curr_inst)))
			(if (member register curr_read)
				#t
				(if (member register curr_write)
					#f
					(is_register_needed? register (cdr rest_lst)))))))
