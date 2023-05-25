(module
  (memory 1)

  (table funcref
    (elem
      $noop
    )
  )

  (type $out-i32 (func (result i32)))

  (func (export "const") (param i32) (result i32)
    (local.get 0)
  )

  (func $first (export "first") (param i32 i32) (result i32)
    (local.get 0)
  )

  (func (export "call-first") (result i32)
    i32.const 0
    i32.const 1
    call $first
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
    local.get 0
    i64.const 0
    i64.eq
    (if (result i64)
      (then
        i64.const 1)
      (else
        local.get 0
        local.get 0
        i64.const 1
        i64.sub
        call $fac-rec
        i64.mul)))

  (func (export "fac-iter") (param i64) (result i64) (local i64 i64)
    local.get 0
    local.set 1
    i64.const 1
    local.set 2
    (block
      (loop
        local.get 1
        i64.const 0
        i64.eq
        (if
          (then
            br 2)
          (else
            local.get 1
            local.get 2
            i64.mul
            local.set 2
            local.get 1
            i64.const 1
            i64.sub
            local.set 1
          )
        )
        br 0
      )
    )
    local.get 2
  )

  (func (export "half-fac") (param i32) (result i32)
    (if (result i32) (i32.eq (local.get 0) (i32.const 0))
      (then (i32.const 1))
      (else (i32.const 0))))

  (func (export "half-fac-64") (param i64) (result i64)
    (if (result i64) (i64.eq (local.get 0) (i64.const 0))
      (then (i64.const 1))
      (else (i64.const 0))))

  (func (export "non-terminating") (result i32)
    (loop (br 0))
    i32.const 0
  )

  (func (export "maybe-non-terminating") (param i32) (result i32)
    (block
      (loop
        (br_if 1 (i32.eq (local.get 0) (i32.const 42)))
        (br 0)
      )
    )
    i32.const 0
  )

  (func (export "test-mem") (param i32) (result i32)
    i32.const 0
    local.get 0
    i32.store
    i32.const 0
    i32.load
    i32.const 1
    i32.add

    )

  (func (export "test-mem2") (result i32)
    i32.const 0
    i32.load
  )

  (func (export "test-size") (result i32)
    memory.size
  )

  (func (export "test-memgrow") (result i32 i32)
    i32.const 1
    memory.grow
    memory.size
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
        (if (result i32) (i32.eq (local.get 0) (i32.const 0))
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
        (if (result i32) (i32.eq (local.get 0) (i32.const 0))
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
    (if (result i32) (i32.eq (local.get 0) (i32.const 0))
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
        (if (result i32) (i32.eq (local.get 0) (i32.const 0))
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
        (if (result i32) (i32.eq (local.get 0) (i32.const 0))
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

  (func (export "params-break") (result i32)
    (local $x i32)
    (i32.const 1)
    (i32.const 2)
    (loop (param i32 i32) (result i32)
      (i32.add)
      (local.tee $x)
      (i32.const 3)
      (local.get $x)
      (i32.const 10)
      (i32.lt_u)
      (br_if 0)
      (drop)
    )
  )

  (func (export "break-multi-value") (result i32 i32 i64)
    (block (result i32 i32 i64)
      (br 0 (i32.const 18) (i32.const -18) (i64.const 18))
      (i32.const 19) (i32.const -19) (i64.const 19)
    )
  )

  (func (export "nesting") (param f32 f32) (result f32)
    (local f32 f32)
    (block
      (loop
        (br_if 1 (f32.eq (local.get 0) (f32.const 0)))
        (local.set 2 (local.get 1))
        (block
          (loop
            (br_if 1 (f32.eq (local.get 2) (f32.const 0)))
            (br_if 3 (f32.lt (local.get 2) (f32.const 0)))
            (local.set 3 (f32.add (local.get 3) (local.get 2)))
            (local.set 2 (f32.sub (local.get 2) (f32.const 2)))
            (br 0)
          )
        )
        (local.set 3 (f32.div (local.get 3) (local.get 0)))
        (local.set 0 (f32.sub (local.get 0) (f32.const 1)))
        (br 0)
      )
    )
    (local.get 3)
  )
)
