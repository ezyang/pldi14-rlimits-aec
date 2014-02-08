# Resource Limits for Haskell: PLDI'14 Artifact Evaluation

This page describes the set of GHC patches and support libraries, as
well as evaluation code, for the paper
[Resource Limits for Haskell](http://example.com).  You can access our artifact via
[this VM image.](http://example.com).  The VM is a minimal Arch Linux
distribution with a complete build of GHC and associated libraries.  The
root password is 'rlimits' (no quotes), and there is a user named
'lambda' in which the software has been placed.

lambda's home directory contains the following:

* `ghc` contains the build-tree of our patched version of GHC 7.8 with
  support for resource limits.  It has been installed to `/usr/local`,
  so if you type `ghc --version` you will be using our copy of GHC.
  You can view the patchset that was applied using `git log` (this
  author likes to think that it is a very clean, orderly patchset), but
  to run our software, you don't have to interact with this directory.

* `rlimits` contains the primary auxiliary library support for resource
  limits, including the information-flow control monad
  Control.Monad.RLimits described in Section 3 of the paper.  As with
  `ghc`, the library has already been installed, so you don't have to
  interact with this directory, except to look at the source code.

* `pldi14-rlimits-aec` contains instructions on how to replicate all
  of the measurements seen in the evaluation section of the paper.
  There are three subfolders, and each contains a `README` with further
  instructions (usually `make` and run).

## Tutorial

This tutorial assumes some familiarity with Haskell, though
non-Haskellers should be able to eyeball what the code is doing
nonetheless.

### Raw resource limits

A good start is to ensure that resource limits has been
installed properly. Here is a short file `1.hs` which imports our
resource limits module.

```haskell
-- 1.hs
import Control.RLimits

main = putStrLn "Hello World!"
```

You can build the file using `ghc -O --make 1.hs`.

`Control.RLimits` provides the low-level access to the resource-limits
API as described in section 2.  We can write a short program which
creates a resource container (`newRC`), switches into it (`withRC`), and
exhausts its space resources (for contrived examples, this can be quite
tricky; more on this later).

```haskell
-- 2.hs
import Control.RLimits
import Control.Exception

main, program :: IO ()

main = do
    parent_rc <- getCurrentRC
    rc <- newRC 200 parent_rc
    withRC rc (loop [1])

loop n = do
    n <- evaluate n
    loop (n ++ n)
```

When you run this program, it exits with a heap overflow.

Remarks on the code: the number passed to `newRC` is the number of
blocks that the resource container is to be allocated.  The size of a
block in GHC is 4096 KB.  Empirically (this is described in the paper),
the true memory usage of a container allocate N space will be up to 2N.
There is also a hierarchical structure to containers (not in the paper);
at the moment, this is only used internally, and has no effect on the
semantics of resource containers.

At this point, you might try having some fun trying to build some of
your own space hogs.  Some care should be taken: the most common reasons
for a limit to not be hit when you expect it to are (1) the program is
time-bound, not space-bound, and (2) the costs are not being charged to
the container you think they are being charged to.  In particular, make
sure you build your programs with `-O`.  If you tweak the previous
example:

```haskell
loop n = do
    evaluate n
    loop (n ++ n)
```

...unoptimized, the cost of this loop is charged to the top-level, and
not the most recently allocated resource container.  This is because the
function desugars to `evaluate n >> loop (n ++ n)`, so what an
unoptimized Haskell program is doing is literally building up a tree of
IO actions, before actually executing them inside a `withRC`.  These
quirks are less likely to show up when resource limits are being used in
real-world settings (where space blowup is due to external output).  The
`accuracy` in the evaluations demonstrates other tricks for making
contrived examples work properly.  `withRC1`, described in the paper,
is another effective technique for ensuring computations happen where
you want them to.

Another feature (not described in the paper), is the ability to receive
callbacks when certain thresholds are met.  This can be done with the
`listenRC` function:

```haskell
-- 3.hs
import Control.RLimits
import Control.Exception

main, program :: IO ()

main = do
    parent_rc <- getCurrentRC
    rc <- newRC 200 parent_rc
    listenRC rc 100 (putStrLn "Half way gone")
    withRC rc $ loop [1]

loop n = do
    n <- evaluate n
    loop (n ++ n)
```

Listeners are useful for applying soft limits, where a thread can be
issued an advisory that it is running out of space before it hits a hard
exception.

### Resource limits monad

We offer a resource limits monad `CM`, in `Control.Monad.RLimits`, for
keeping track of what pointers threads have to various resource
containers.  You can start a computation in the `CM` monad using
`startCM`:

```haskell
-- 4.hs
import Control.Monad.RLimits

croak :: String -> CM ()
croak = liftIOTCB . putStrLn

main = startCM $ do
    croak "Hello"
```

The most important operation of the RC monad is `rcFork`, which is the
mechanism by which you can enforce isolation between containers.  In
the following example, notice that while both access to the killed
container fail, the main thread is able to proceed (and print `fine`)
independent of the failure of the forked thread.

```haskell
import Control.Monad.RLimits

croak :: String -> CM ()
croak = liftIOTCB . putStrLn

main = startCM $ do
    c <- getCurrentLabel
    rc <- newRC 100
    rcKill rc
    rcFork (ss rc `su` c) (withRC rc (croak "1"))
    croak "fine"
    withRC rc (croak "2")
```

What is the first argument to `rcFork`?  This argument specifies
what resource containers the thread may access (read as "the
singleton set (`ss`) containing `rc` unioned (`su`) with the
current set of accessible containers").  This same format is also
used for `RCRef` and `RCLMVar`, which means of cross-thread
communication.

For a full example, check out the `prisoners` subfolder in the
evaluation folder.

## Known infelicities

This resource limits system has been most strenously tested on a
`quick`, single-threaded build, and the authors do not claim to have
squashed all bugs, especially in other configurations.

Features which are known not to work:

* 32-bit architectures

* GHCi

* Compacting garbage collection

Known bugs:

* We leak a small amount of memory for every resource container
  allocated.  Fixing this is not conceptually difficult but requires
  how we handle resource container pointers to be restructured.
