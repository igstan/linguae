# JIT Hello World for Apple Silicon

## Resources

  - [Antonio Cuni — How to write a JIT compiler in 30 minutes](https://www.youtube.com/watch?v=DKns_rH8rrg)
  - [Porting Just-In-Time Compilers to Apple Silicon][apple-jit]
  - [QEMU Patch: Fix execution on Apple Silicon](https://lists.gnu.org/archive/html/qemu-devel/2021-01/msg02555.html)
  - [Gist for Linux/x86](https://gist.github.com/markmont/dcd20d632fa753438f6fc1b3bb3711ec)

## Notes

For some reason, the executable doesn't seem to need the `com.apple.security.cs.allow-jit`
entitlement, as mentioned in [Porting Just-In-Time Compilers to Apple Silicon][apple-jit].

[apple-jit]: https://developer.apple.com/documentation/apple-silicon/porting-just-in-time-compilers-to-apple-silicon
