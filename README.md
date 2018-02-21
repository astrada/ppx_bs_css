ppx\_bs\_css
============

A ppx rewriter for CSS expressions.

Parses a CSS string and produces a declaration block compatible with
[typed-css-core](https://github.com/glennsl/bs-typed-css/tree/master/packages/core).

### Build

    npm install -g esy
    esy install
    esy build
    # to build tests
    esy jbuilder build test/test_suite.exe
    # to run tests
    esy _build/default/test/test_suite.exe

### Example

```ocaml
let declarationBlock =
  let open TypedGlamor;
  [%css
    {|
      color: red;
      background-color: test;
      margin: auto 0 10px 1em;
      border-bottom: thin dashed #eee;
      border-right-color: rgb(1, 0, 1);
      width: 70%;
      background: url(http://example.com/test.jpg)
    |}
  ];
```

See also:
[ppx_bs_css-examples](https://github.com/astrada/ppx_bs_css-examples).

