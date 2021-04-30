[User Guide](../../wiki/UserGuide) | [NuGet](https://www.nuget.org/packages/Unquote/) | [Release Notes](../../wiki/ReleaseNotes) | [Issues](../../issues)

---

Unquote is not just another DSL or API for making unit test assertions. Instead, assertions are written as plain, statically-checked F# [quoted expressions](http://msdn.microsoft.com/en-us/library/dd233212.aspx) and test failure messages are given as step-by-step F# expression evaluations.

Unquote integrates configuration-free with all exception-based unit testing frameworks including xUnit.net, NUnit, MbUnit, Fuchu, and MSTest. For example, the following failing xUnit.net test

```
[<Fact>]
let ``demo Unquote xUnit support`` () =
    test <@ [3; 2; 1; 0] |> List.map ((+) 1) = [1 + 3..1 + 0] @>
```

produces the following failure message

```
Test 'Module.demo Unquote xUnit support' failed: 

[3; 2; 1; 0] |> List.map ((+) 1) = [1 + 3..1 + 0]
[4; 3; 2; 1] = [4..1]
[4; 3; 2; 1] = []
false

	C:\File.fs(28,0): at Module.demo Unquote xUnit support()
```

Unquote may even be used within FSI sessions, enabling the direct migration of ad-hoc FSI tests during interactive development to formal test suites.

In addition to `test : Quotations.Expr<bool> -> unit` used for performing most assertions, Unquote has convenience operators `=!`, `>!`, `<!`, `>=!`, `<=!`, and `<>!` for performing  simple comparison assertions. And finally, Unquote has operators `raises<'a when 'a :> exn> : Quotations.Expr -> unit` and `raisesWith : Expr -> (#exn -> Expr<bool>) -> unit` for asserting whether a quoted expression raises an expected exception.

At the heart of Unquote, are (public) operators for decompiling, evaluating, and incrementally reducing F# Quotations. Unquote can decompile many F# quoted expressions as single line, non-light syntax strings. See [Decompiler Features](../../wiki/DecompilerFeatures) for a list of notable decompiler features.

The incremental evaluator performs reduction steps until a non-reducible expression is reached. During a reduction step, an expression will be reduced if all of its subexpressions are already reduced, recursively reducing the sub-expressions otherwise. Hence order of evaluation is not strictly adhered to, but shouldn't be a problem in practice since assertion expressions are not expected to depend on order of evaluation side effects. Except for sequential expressions, which are evaluated from left to right, and control structures and boolean operator expressions, which follow valid branch paths and short-circuiting rules. If a test expression throws an exception, the test will fail and print each reduction step up to the point of the exception, and then print the exception.

Please see the Wiki page GettingStarted for a simple guide to getting started with Unquote.

Unquote was inspired by [Groovy Power Asserts](http://dontmindthelanguage.wordpress.com/2009/12/11/groovy-1-7-power-assert/).

---

You are welcome to [Pay What You Want](https://www.paypal.com/cgi-bin/webscr?cmd=_s-xclick&hosted_button_id=ZNFZMKQF77YRC) for Unquote via PayPal.

Copyright 2011-2021 [Swensen Software](http://www.swensensoftware.com)
