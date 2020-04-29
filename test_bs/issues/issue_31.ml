open Css
let test = [%css {|
  .ant-breadcrumb{
    padding-top: 20px;
  }
|}]

let equal = [
  selector
    ".ant-breadcrumb"
    [paddingTop(px 20)]
]
let _ = assert (test = equal)
