# steam
Search Tags in Erlang Application or Module

## Overview ##
`steam` is a module detecting Debian package tags on Erlang projects. 

Debian package tags are used because of their completeness compared to many other distributions. However those tags can be easily transcoded to anothers if needed.

Reference is [Debtags](http://anonscm.debian.org/cgit/debtags/vocabulary.git/tree/debian-packages).

## API ##
### Search tags in a project ###

Extracting Debian tags use `steam:tags/1`. 

Argument is the path to the Erlang project root directory.

Returns `{ok, ListOfTags}` or `{error, Reason}`.

```
1> steam:tags("/path/to/root/project/directory/").
{ok,['implemented-in::erlang','protocol::ip',
     'protocol::ssh','scope::suite','use::organizing']}

```

### Listing facets ###
Debian tag classification use [Facets](https://en.wikipedia.org/wiki/Faceted_classification).

Listing facets used in tags :
```
1> steam:facets().
```

Listing all potential facets :

```
1> steam:facets(all).
```

### Listing tags ###

Listing available tags :
```
1> steam:tags().
```

## Quick Start ##

```
git clone git://github.com/crownedgrouse/steam.git
cd steam
make
erl -pa `pwd`/ebin
```
or
```
git clone git://github.com/crownedgrouse/steam.git
cd steam
make shell
```

## Contributing ##

Contributions are welcome. Please use pull-requests.

