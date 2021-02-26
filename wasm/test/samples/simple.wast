(module
  (memory 1)
  ;; Recursive factorial
  (func (export "const") (param i32) (result i32)
    (get_local 0)
  )

  (func (export "noop") (result i32)
    (i32.const 0)
  )

  (func (export "half-fac") (param i32) (result i32)
    (if (result i32) (i32.eq (get_local 0) (i32.const 0))
      (then (i32.const 1))
      (else (i32.const 0))))

  (func (export "half-fac-64") (param i64) (result i64)
    (if (result i64) (i64.eq (get_local 0) (i64.const 0))
      (then (i64.const 1))
      (else (i64.const 0))))

  (func (export "test-mem") (param i32) (result i32)
    i32.const 0
    get_local 0
    i32.store
    i32.const 0
    i32.load
    i32.const 1
    i32.add

    ))
