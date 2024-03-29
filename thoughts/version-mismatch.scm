(post
 "Version Mismatch"
 "2024/03/02 18:19 +0000"
 {just
  I've noticed an increasing tenancy for projects to stick to a major version of 0.
  I found an especially bad case in the README of {{a {href https://github.com/kspalaiologos/kamilalisp}} Kamila Lisp}:
  {q Every release will be tagged as 0.3.x.y where a bump of x signifies a breaking change, while the bump of y signifies a non-breaking change.}
  To keep the major version as 0, they had to add an extra sub-version below the patch version and shift the meanings down by one.
  This exact effect can be achieved through regular versioning by omitting the leading 0 from this new scheme.
  I think the issue generally comes from a mismatch between how projects are developed and how version numbers work.
  Most open source projects are developed a single patch at a time, with each patch being released to the public instantly, so there is no instant at which it makes sense to break off into a new major version.
  By contrast, commercial software releases features in large batches, and so major versions make sense.
  Another reason for sticking to version 0 is that an increase in major version is a kind of commitment.
  There is an expectation that some amount of maintenance still happens on older major versions, which may be beyond the scope of a given project.
  I urge developers to be aware of the reason a major version may not change, and then to actually increment the major version when it is logical to do so, rather than treating it as untouchable.})
