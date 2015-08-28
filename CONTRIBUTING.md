# Instructions for contributing

This guide contains instructions for collaborators to contribute with
the `legTools` package.

The general guidelines for contributing to any of the projects in this
GitLab can be found in [Boas práticas do uso do Git e GitLab no LEG][]
(in portuguese only).

## Workflow

In `legTools` we use the Gitflow workflow, which can be viewed in these
two links: [A successful Git branching model][], and
[Atlassian tutorial][].

Basically, all the ongoing development is made in the `devel`
branch. New features and bug fixes are made in additional (parallell)
branches such like `feature/<num>`, where `<num>` is an incremental
number. Feature branches are merged with the `devel` branch. When a new
version is supposed to be ready for release, it is moved to the
`release` branch, where final tests are made to guarantee it is
stable. Only then it is moved to the `master` branch, where it receives
a tag with the version number.

This way, users may opt to install the stable version, from the `master`
branch, or they may install the development version from the `devel`
branch.

To contribute to the project, please review the commits in the `devel`
branch to make sure that what you are trying to do is not already being
done. Then create a new branch, based on the `devel` branch, and make
changes in this branch. When you are ready to submit your changes,
please open a Merge Request (MR), and explain in details your
implementation, and how this could be beneficial to the project.

Also, if you're not sure about how to do something, but have an idea for
the project, feel free to open an issue and explain what you want from
there.

## General rules for commits

Commits are a very important part of git. The four most important rules
to follow are:

1. **Commit often**
2. **Don't commit half-done work**
3. **Test before you commit**
4. **Write good commit messages**

For more detailed explanation and specific guidelines, see
[Criando commits][] in [Boas práticas do uso do Git e GitLab no LEG][],
and [Version Control Best Practices][].

****

Fell free to contact us (via email or an issue) if you have any doubts
about contributing.

<!-- links -->

[A successful Git branching model]: http://nvie.com/posts/a-successful-git-branching-model/
[Atlassian tutorial]: https://www.atlassian.com/git/tutorials/comparing-workflows/feature-branch-workflow
[Boas práticas do uso do Git e GitLab no LEG]: http://git.leg.ufpr.br/leg/gitlab-rautu/blob/master/CONTRIBUTING.md
[Version Control Best Practices]: http://www.git-tower.com/learn/git/ebook/command-line/appendix/best-practices
[Criando commits]: http://git.leg.ufpr.br/leg/gitlab-rautu/blob/master/CONTRIBUTING.md#criando-commits
