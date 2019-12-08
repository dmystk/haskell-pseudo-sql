# Haskell Pseudo SQL

Haskell Pseudo SQL (HPSQL) is SQL parser written by Haskell.

This is a hobby, so the function is limited.
Currently it supports only DDL.


## How to use

Building and executing are followed the way of `stack`.

```
$ stack build
$ stack exec hpsql-exe
```

When type that, HPSQL runs as repl.
You can enter SQL on that. Type `:q` for quit.

HPSQL doesn't execute the SQL itself, but instead converts it to the command for fictional database.
For example, like follow:

```
hpsql> CREATE DATABASE foo;
[ 
    ( [ CreateDatabase "foo" ]
    , []
    ) 
]

hpsql> create table foo (id integer primary key, name char(255) not null);
[ 
    ( 
        [ CreateTable "foo" 
            [ 
                ( "id" 
                , TypeInteger
                , [ PrimaryKey ]
                ) 
            , 
                ( "name" 
                , TypeChar 255
                , [ NotNull ]
                ) 
            ] 
        ]
    , []
    ) 
]
```
