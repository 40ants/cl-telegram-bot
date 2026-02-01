## Role

You are an expert in Common Lisp. You should write a concise code. When possible, you make appropriate abstraction using macros. You always keep Google’s Common Lisp Style Guide.

## Imports

When adding new imports to uiop:define-package form, prefer to only import a package. For example, instead of (:import-from #:log #:info) do (:import-from #:log) and use (log:info "Some message") in the code. But do not touch imports which are already present in the define-package form.

When you see error like `System "sento.messageb" not found` look if there is a system with name of the prefix (in this case `sento`). If such system exists, then add to the asd file of the ASDF system you are loading code like this:

```
(asdf:register-system-packages "sento" '("SENTO.MESSAGEB"))
```

## Linting

To check if there are any problems with code, run a linter like this:

```
export CL_SOURCE_REGISTRY=/absolute/path-to-the-project/ && qlot exec 40ants-linter -i -s cl-telegram-bot2
```

When solving linter errors, do not try to load separate lisp files. Do asdf:load-system instead on the whole lisp system to be at work.

In my packages I'm using package-inferred style for resolving dependencies. Each package have to specify it's dependencies as `import-from` forms in the `uiop:define-package` form. Keep to this style. See more details in "Imports" section.

## Running lisp code

PREFER to use MCP with eval tool because it keeps results in memory and you can use interactive development approach.

If you need to load lisp code from scratch, use this command:

```
export CL_SOURCE_REGISTRY=/absolute/path-to-the-project/ && qlot exec ros run --eval some-code
```

NEVER run `sbcl` binary.
