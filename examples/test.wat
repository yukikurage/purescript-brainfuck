(memory $0 4 4)
(export "main" (func $main))
(func $main (; 0 ;)  (param $x i32) (local $0 i32)
  (local.set $0
    (i32.load offset=0
      (local.get $x)
    )
  )
  (i32.store8
    (local.get $0)
    (i32.const 10)
  )
  (local.set $0
    (i32.add
      (local.get $x)
      (local.get $0)
    )
  )
  (i32.store8
    (local.get $0)
    (i32.const 10)
  )
)
