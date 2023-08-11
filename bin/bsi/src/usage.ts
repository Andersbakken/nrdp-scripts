export const usage = `bsi [...options] [build]
  [--help|-h]                                     Display this help
  [--verbose|-v]                                  Be more verbose

  [--project|-p] <project>                        Use this project instead of deducing it from the file system
  [--project=<project>|-p=<project>|-p<project>

  [--build|-b] <build>                            Use this build (default master)
  [--build=<build>|-b=<build>|-b<build>]

  [--commit|-c] <sha>                             Find build by git commit
  [--commit=<sha>|-c=<sha>|-o<sha>

  [--file|-f] <file>                              Try to deduce build number from file.
  [--file=<file>|-f=<file>|-f<file>

  [--url|-u] <url>                                Try to deduce build number from url contents.
  [--url=<url>|-u=<url>|-u<url>

  [--env|-e] <env>                                Use this env (default prod, (prod, debug or prod.assertions))
  [--env=<env>|-e=<env>|-e<env>
  [--prod|-P]                                     Use prod env
  [--prod-assertions|-a]                          Use prod.assertions env
  [--debug|-d]                                    Use debug env

  [--info|-i] <optional field>                    Print info about build, optionally only certain fields
  [--info=<field>|-i=<field>|-i<field>]           (use "all" to see all fields or "list" to "list" them)

  [--download|-D]                                 Download build (by default save to file of same name)
  [--stdout|-1]                                   Print to stdout, implies --download
  [--stderr|-2]                                   Print to stderr, implies --download
  [--output|-o] <file>                            Save contents to this file, implies --download
  [--output=<file>|-o=<file>|-o<file>
`;
