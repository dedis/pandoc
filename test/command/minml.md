```
% pandoc -f minml -t html --wrap=none
h1{ id=title }[Title]
p[Hello em[world].]
^D
<h1 id="title">Title</h1>
<p>Hello <em>world</em>.</p>
```

```
% pandoc -f html -t minml --wrap=none
<p>Hello <em>world</em>.</p>
^D
p[Hello em[world].]
```

```
% pandoc -f minml -t native
Pandoc[meta[]blocks[Para[Hello Emph[world].]]]
^D
[ Para
    [ Str "Hello" , Space , Emph [ Str "world" ] , Str "." ]
]
```

```
% pandoc -f xml -t minml --wrap=none
<Pandoc><meta/><blocks><Para>Hello <Emph>world</Emph>.</Para></blocks></Pandoc>
^D
p[Hello em[world].]
```

```
% pandoc -t plain command/minml.minml
^D
Hello world.
```

```
% pandoc -f minml -t xml | pandoc -f xml -t html --wrap=none
p[Hello em[world].]
^D
<p>Hello <em>world</em>.</p>
```

```
% pandoc -f html -t minml | pandoc -f minml -t html --wrap=none
<p>Use <code>a[b] &lt; c</code>.</p>
^D
<p>Use <code>a[b] &lt; c</code>.</p>
```
