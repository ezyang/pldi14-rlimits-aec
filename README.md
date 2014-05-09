# Resource Limits for Haskell: PLDI'14 Artifact Evaluation

**Warning:** Since the PLDI artifact evaluation, development has
progressed further and some bugs have been fixed.  The VM image
has not been updated for historical interest, but be aware!

This page describes the set of GHC patches and support libraries, as
well as evaluation code, for the paper
[Resource Limits for Haskell](http://ezyang.com/papers/ezyang14-rlimits.pdf).  You can access our artifact via
[this 7 GB VM image (Warning: this is an SCS lab hosted machine.  We have turned off logging though!)](http://hs01.scs.stanford.edu/rlimits.vmdk).  The VM is a minimal Arch Linux
distribution with a complete build of GHC and associated libraries.
There is a user 'lambda' with password 'rlimits'. (The root password
is also the same).

lambda's home directory contains the following:

* `pldi14-rlimits-aec` contains instructions on how to replicate all
  of the measurements seen in the evaluation section of the paper.
  There are three subfolders, and each contains a `README` with further
  instructions.  You can also view the READMEs by browsing this Git
  repository.

* `ghc` contains the build-tree of our patched version of GHC 7.8 with
  support for resource limits.  It has been installed to `/usr/local`,
  so if you type `ghc --version` you will be using our copy of GHC.
  You can view the patchset that was applied using `git log`, but
  to run our software, you don't have to interact with this directory.

* `rlimits` contains the primary auxiliary library support for resource
  limits, including the information-flow control monad
  Control.Monad.RLimits described in Section 3 of the paper.  As with
  `ghc`, the library has already been installed, so you don't have to
  interact with this directory, except to look at the source code.
  This code is available at https://github.com/ezyang/rlimits

## Tutorial

This tutorial assumes some familiarity with Haskell.

### Raw resource limits

A good start is to ensure that resource limits has been
installed properly. Here is a short file `1.hs` which imports our
resource limits module.

```haskell
-- 1.hs
import Control.RLimits

main = putStrLn "Hello World!"
```

You can build the file using `ghc -O --make 1.hs`, which results in an
executable named `1`.

`Control.RLimits` provides the low-level access to the resource-limits
API as described in section 2.  We can write a short program which
creates a resource container (`newRC`), switches into it (`withRC`), and
exhausts its space resources (for synthetic examples, this can be a little
tricky; more on this later):

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
    _ <- evaluate (length n)
    loop (n ++ n)
```

When you run this program, it exits with a heap overflow, as
the loop repeatedly doubles the size of a list.

Differently from the paper, `newRC` accepts a number of blocks to be
allocated to the resource container.  The size of a block in GHC is 4
KB.  You also have to provide a parent resource container to associate
with the container; at the moment, this is only used internally, and has
no effect on the semantics of resource containers.

When constructing synthetic examples, some care should be taken: the
most common reasons for a limit to not be hit when you expect it to are
(1) the program is time-bound, not space-bound (check `top`), and (2)
the costs are not being charged to the container you think they are
being charged to.  These difficulties are closely related to the
difficulty of performing microbenchmarks in Haskell: it can be quite
difficult to tell when a computation is happening, and our paper does
not introduce any new special forms to Haskell to make this easier.
Usually, computation that is done by the IO monad will be attributed
properly, but you still need to be careful.  Here is a slightly tweaked
version of the previous example:

```haskell
loop n = do
    evaluate (length n)
    loop (n ++ n)
```

When compiled without optimizations, the cost of this loop is charged to
the top-level, and not the most recently allocated resource container.
What happened? It turns out these two different uses of do-notation
compile differently: in the second case, the unoptimized Haskell program
builds a tree of IO actions, before actually running the actions in the
`withRC`.  If you need to run a pure computation inside a container,
use `withRC1`.

Something else you can do (not described in the paper) is the ability to
receive callbacks when certain thresholds are met.  This can be done
with the `listenRC` function:

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
    _ <- evaluate (length n)
    loop (n ++ n)
```

Listeners are useful for applying soft limits, where a thread can be
issued an advisory that it is running out of space before it hits a hard
exception.

### Resource limits monad

Using the raw resource limits API requires quite a bit of care,

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

Known bugs in the VM (all of these bugs have since been fixed in the
Git repository, but I haven't updated the VM with the new software):

* We leak a small amount of memory for every resource container
  allocated.

* The raw resource limits interface has an unsafe function `killRC`
  (distinct from the `killRC` in the monadic interface). This function
  is advisory: when a container is killed, the user makes a promise that
  the container will no longer be used with `withRC`; behavior is
  undefined otherwise.  We could probably make this interface a little
  less pointy.

* Our HEAP_ALLOCED patch does not work with SplitObjs; thus be sure to
  compile GHC with that setting turned off, and to not use
  `--split-objs` when compiling libraries. See also:
  https://ghc.haskell.org/trac/ghc/ticket/8199#comment:20
