ppx\_bs\_css
============

A ppx rewriter for CSS expressions.

Parses a CSS string and produces a declaration block compatible with
[bs-css](https://github.com/SentiaAnalytics/bs-css), and
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

Basic `bs-css` rules (ReasonML syntax):

```ocaml
/* rules has type list(Css.rule) */
let rules =
  let open Css;
  [%style
    {|
      color: red;
      background-color: white;
      margin: auto 0 10px 1em;
      border-bottom: thin dashed #eee;
      border-right-color: rgb(1, 0, 1);
      width: 70%;
      background: url(http://example.com/test.jpg)
    |}
  ];
```

`bs-css` rules with selectors (ReasonML syntax):

```ocaml
/* rules has type list(Css.rule) */
let css =
  let open Css;
  [%css
    {|
      {
        color: red;
      }

      :hover {
        color: blue;
      }
    |}
  ];
```

`bs-css` keyframes (ReasonML syntax):

```ocaml
let bounces =
  let open Css;
  [%style
    {|
      @keyframes {
        from { transform: scale(0.1, 0.1); opacity: 0.0; }
        60% { transform: scale(1.2, 1.2); opacity: 1.0; }
        to { transform: scale(1.0, 1.0); opacity: 1.0; }
      }
    |}
  ];
let bounce = List.hd(bounces);
let stylesWithAnimation =
  let open Css;
  [%style
    {|
      animation-name: bounce;
      animation-duration: 2000;
      width: 50px;
      height: 50px;
      background-color: rgb(255, 0, 0)
    |}
  ];
```

Basic `typed-glamor` declaration block (ReasonML syntax):

```ocaml
/* declarations has type list(TypedGlamor.Core.declaration) */
let declarations =
  let open TypedGlamor;
  [%style
    {typed|
      color: red;
      background-color: white;
      margin: auto 0 10px 1em;
      border-bottom: thin dashed #eee;
      border-right-color: rgb(1, 0, 1);
      width: 70%;
      background: url(http://example.com/test.jpg)
    |typed}
  ];
```

See also:
[ppx_bs_css-examples](https://github.com/astrada/ppx_bs_css-examples).

