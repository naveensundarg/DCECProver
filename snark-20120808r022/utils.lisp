(defun get-proof-clauses ()

  (mapcar (lambda (row) (let ((wff (snark:row-wff row)))
                          (if (atom wff) 
                              wff (snark:args wff))))
          (remove-if-not (lambda (row)  
                           (equalp 'snark::ASSERTION (snark:row-reason row)))
                         (snark:row-ancestry (snark:proof)))))


