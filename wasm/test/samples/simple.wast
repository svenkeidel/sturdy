(module
  (memory 1)

  (table anyfunc
    (elem
      $noop
    )
  )

  (type $out-i32 (func (result i32)))

  (func (export "const") (param i32) (result i32)
    (get_local 0)
  )

  (func $noop (export "noop") (result i32)
    (i32.const 0)
  )

  (func $test1 (result i32)
    (i32.const 1)
    (call $noop)
    (i32.add)
  )

  (func (export "test2") (result i32)
    (i32.const 2)
    (call $test1)
    (i32.add)
  )

  (func $fac-rec (export "fac-rec") (param i64) (result i64)
    get_local 0
    i64.const 0
    i64.eq
    (if (result i64)
      (then
        i64.const 1)
      (else
        get_local 0
        get_local 0
        i64.const 1
        i64.sub
        call $fac-rec
        i64.mul)))

  (func (export "fac-iter") (param i64) (result i64) (local i64 i64)
    get_local 0
    set_local 1
    i64.const 1
    set_local 2
    (block
      (loop
        get_local 1
        i64.const 0
        i64.eq
        (if
          (then
            br 2)
          (else
            get_local 1
            get_local 2
            i64.mul
            set_local 2
            get_local 1
            i64.const 1
            i64.sub
            set_local 1
          )
        )
        br 0
      )
    )
    get_local 2
  )

  (func (export "half-fac") (param i32) (result i32)
    (if (result i32) (i32.eq (get_local 0) (i32.const 0))
      (then (i32.const 1))
      (else (i32.const 0))))

  (func (export "half-fac-64") (param i64) (result i64)
    (if (result i64) (i64.eq (get_local 0) (i64.const 0))
      (then (i64.const 1))
      (else (i64.const 0))))

  (func (export "non-terminating") (result i32)
    (loop (br 0))
    i32.const 0
  )

  (func (export "maybe-non-terminating") (param i32) (result i32)
    (block
      (loop
        (br_if 1 (i32.eq (get_local 0) (i32.const 42)))
        (br 0)
      )
    )
    i32.const 0
  )

  (func (export "test-mem") (param i32) (result i32)
    i32.const 0
    get_local 0
    i32.store
    i32.const 0
    i32.load
    i32.const 1
    i32.add

    )

  (func (export "test-br1") (result i32)
    (block (result i32)
           i32.const 42
           br 0)
  )

  (func (export "test-br2") (result i32)
    (block (result i32)
      (block (result i32)
        i32.const 42
        br 1))
    i32.const 1
    i32.add
  )

  (func (export "test-br3") (param i32) (result i32)
    (block (result i32)
      (block (result i32)
        (if (result i32) (i32.eq (get_local 0) (i32.const 0))
          (then
            i32.const 42
            br 0
          )
          (else
            i32.const 43
            br 1
          )
        )
      )
    )
  )

  (func (export "test-br-and-return") (param i32) (result i32)
    (block (result i32)
      (block (result i32)
        (if (result i32) (i32.eq (get_local 0) (i32.const 0))
          (then
            i32.const 42
            return
          )
          (else
            i32.const 43
            br 1
          )
        )
      )
    )
  )

  (func (export "test-unreachable") (result i32)
    i32.const 42
    return
    unreachable
  )

  (func (export "test-unreachable2") (result i32)
    (block (result i32)
      i32.const 42
      return
    )
    unreachable
  )

  (func (export "test-unreachable3") (result i32)
    (block (result i32)
      i32.const 42
      br 1
    )
    unreachable
  )

  (func (export "test-unreachable4") (result i32)
    (block (result i32)
      i32.const 42
      br 0
    )
    unreachable
  )

  (func (export "test-unreachable5") (param i32) (result i32)
    (if (result i32) (i32.eq (get_local 0) (i32.const 0))
      (then
        i32.const 42
        br 1
      )
      (else
        i32.const 43
        br 1
      )
    )
    unreachable
  )

  (func (export "test-br-and-return3") (param i32) (result i32)
      (block (result i32)
        (if (result i32) (i32.eq (get_local 0) (i32.const 0))
          (then
            i32.const 42
            br 1
          )
          (else
            i32.const 43
            br 1
          )
        )
        unreachable
      )
    )

  (func (export "test-br-and-return2") (param i32) (result i32)
    (block (result i32)
      (block (result i32)
        (if (result i32) (i32.eq (get_local 0) (i32.const 0))
          (then
            i32.const 42
            return
          )
          (else
            i32.const 43
            br 2
          )
        )
      )
      unreachable
    )
  )

  (func (export "test-call-indirect") (result i32)
    (call_indirect (type $out-i32) (i32.const 0))
  )
)
