# How to contribute to Numsense

Numsense is written in F#, using simple concepts and elementary language constructs. If you're **new to F#**, the Numsense project offers a **unique opportunity** to work with the language in a straightforward, easy-to-understand code base.

Numsense is currently being developed in F# 4 on .NET 4.5, using Visual Studio 2015 with xUnit.net as the unit testing framework. So far, all development has been done with TDD, so there's a high degree of code coverage, and it would be preferable to keep it that way.

## Contributor Licence Agreement

If you want to contribute to Numsense, you must sign the Contributor Licence Agreement (CLA). GitHub will notify you when it's time to do that, but you can peruse [the text of the CLA](https://gist.github.com/ploeh/9fef4fe35e21a44684e5) in advance, if you'd like to.

## Features

You can always find a list of desired features, defects to be corrected, and other work, in the GitHub projects [issue list](https://github.com/ploeh/Numsense/issues). Issues marked with [the jump-in tag](http://nikcodes.com/2013/05/10/new-contributor-jump-in) are deemed particularly friendly to newcomers. These are good candidates to attempt if you are just getting started with Numsense.

Apart from the issues listed in the issue list, Numsense will benefit from addtional (natural) language support. Although it was bootstrapped supporting only English and Danish, it has always been the intention to offer as many language conversions as possible. If you know a language not already covered by Numsense, feel free to submit a Pull Request (see below).

## Dependencies

All binaries (such as xUnit.net) are included as NuGet packages in the source control repository under the `./Packages` directory. All additional binaries not part of .NET 4.5 must also be added to the repository, so that it would be possible to pull down the repository and immediately be able to compile and run all tests.

Numsense itself only depends on *FSharp.Core*, and should be kept as free of dependencies as possible. The unit test libraries, on the other hand, have more dependencies.

## Verification

There is currently a single Visual Studio solution to be found in the root directory, but be aware that the final verification step before pushing to NuGet is to successfully build the entire project using the `Build.fsx` FAKE build script.

Assuming that you have *F# Interactive* (FSI) in your path, you can run the build this way:

```
$ fsi Build.fsx
```

This build script will

- clean the working directory of oncommitted files
- compile the code
- run all tests

If you want to issue a Pull Request, please first make sure that the verification succeeds as described here.

## Pull Requests

When developing for Numsense, please respect the coding style already present. Look around in the source code to get a feel for it.

Please keep **line lengths under 80 characters**. Wide lines don't fit into the standard GitHub code listing window, so it requires vertical scrolling to review.

Also, please follow the [Open Source Contribution Etiquette](http://tirania.org/blog/archive/2010/Dec-31.html). Numsense is a fairly typical open source project: if you want to contribute, start by [creating a fork](https://help.github.com/articles/fork-a-repo) and [sending a pull request](https://help.github.com/articles/using-pull-requests) when you have something you wish to commit. When creating pull requests, please keep the Single Responsibility Principle in mind. A pull request that does a single thing well is more likely to be accepted. See also the articles [The Rules of the Open Road](http://blog.half-ogre.com/posts/software/rules-of-the-open-road) and [10 tips for better Pull Requests](http://blog.ploeh.dk/2015/01/15/10-tips-for-better-pull-requests) for more tips on working with OSS and Pull Requests.

For complex pull requests, you are encouraged to first start a discussion on the [issue list](https://github.com/ploeh/ZeroToNine/issues). This can save you time, because the Numsense regulars can help verify your idea, or point you in the right direction.

Some existing issues are marked with [the jump-in tag](http://nikcodes.com/2013/05/10/new-contributor-jump-in). These are good candidates to attempt, if you are just getting started with Numsense.

When you submit a pull request, you can expect a response within a day or two. We (the maintainers of Numsense) have day jobs, so we may not be able to immediately review your pull request, but we do make it a priority. Also keep in mind that we may not be in your time zone.

If your pull request adds support for a new (natural) language, however, we'll have to find a second reviewer who also knows that language. That may take time.

Most likely, when we review pull requests, we will make suggestions for improvement. This is normal, so don't interpret it as though we don't like your pull request. On the contrary, if we agree on the overall goal of the pull request, we want to work *with* you to make it a success.
