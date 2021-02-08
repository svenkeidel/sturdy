(module
  ;; Recursive factorial
  (func (export "const") (param i32) (result i32)
    (get_local 0)
  )

  (func (export "noop") (result i32)
    (i32.const 0)
  )    
)
