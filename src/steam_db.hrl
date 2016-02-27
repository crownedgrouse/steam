%%% File: steam_db.hrl
%%% @copyright Eric Pailleau <steam@crownedgrouse.com>
%%% @licence   https://github.com/crownedgrouse/steam/blob/master/LICENCE
%%% @doc 
%%% Steam database 
%%% @end 

-define(Devel, []).
-define(Made_of, []).
-define(Interface, []).
-define(Implemented_in, ['implemented-in::c', 'implemented-in::c++', 
                         'implemented-in::erlang', 'implemented-in::java']).
-define(Works_with, []).
-define(Works_with_format,[]).
-define(Scope, ['scope::suite', 'scope::utility', 'scope::application']).
-define(Role, ['role::program']).
-define(Special, []).
-define(Protocol, ['protocol::corba' , 'protocol::ftp' , 'protocol::http' , 
                   'protocol::ip' , 'protocol::ipv6' , 'protocol::ldap' , 
                   'protocol::sftp' , 'protocol::snmp' , 'protocol::ssh' , 
                   'protocol::ssl' , 'protocol::telnet' , 'protocol::tftp' , 
                   'protocol::udp']).
-define(Uitoolkit,['uitoolkit::wxwidgets']).
-define(Web,['web::application', 'web::server']).
-define(Network,[]).

-spec tags({call | export | application , {atom(), atom(), integer()}}) -> atom() | [].
%%******************************************************************************
%% Facet: devel
%% Description: Software Development
%%  How the package is related to the field of software development

% Tag: devel::lang:c
% Description: C Development

% Tag: devel::lang:c++
% Description: C++ Development

% Tag: devel::lang:erlang
% Description: Erlang Development

% Tag: devel::lang:haskell
% Description: Haskell Development

% Tag: devel::lang:java
% Description: Java Development

% Tag: devel::lang:posix-shell
% Description: POSIX shell

% Tag: devel::library
% Description: Libraries

% Tag: devel::modelling
% Description: Modelling
%  Programs and libraries that support creation of software models
%  with modelling languages like UML or OCL.

% Tag: devel::packaging
% Description: Packaging
%  Tools for packaging software.

% Tag: devel::prettyprint
% Description: Prettyprint
%  Code pretty-printing and indentation/reformatting.

% Tag: devel::profiler
% Description: Profiling
%  Profiling and optimization tools.

% Tag: devel::rcs
% Description: Revision Control
%  RCS (Revision Control System) and SCM (Software Configuration Manager)

% Tag: devel::rpc
% Description: RPC
%  Remote Procedure Call, Network transparent programming

% Tag: devel::runtime
% Description: Runtime Support
%  Runtime environments of various languages and systems.

% Tag: devel::testing-qa
% Description: Testing and QA
%  Tools for software testing and quality assurance.

% Tag: devel::ui-builder
% Description: User Interface
%  Tools for designing user interfaces.

% Tag: devel::web
% Description: Web
%  Web-centric frameworks, CGI libraries and other web-specific development
%  tools.


%%******************************************************************************
%% Facet: made-of
%% Description: Made Of
%%  The languages or data formats used to make the package

% Tag: made-of::audio
% Description: Audio

% Tag: made-of::dictionary
% Description: Dictionary

% Tag: made-of::font
% Description: Font

% Tag: made-of::html
% Description: HTML, Hypertext Markup Language

% Tag: made-of::icons
% Description: Icons

% Tag: made-of::info
% Description: Documentation in Info Format

% Tag: made-of::man
% Description: Manuals in Nroff Format

% Tag: made-of::pdf
% Description: PDF Documents

% Tag: made-of::postscript
% Description: PostScript

% Tag: made-of::sgml
% Description: SGML, Standard Generalized Markup Language

% Tag: made-of::svg
% Description: SVG, Scalable Vector Graphics

% Tag: made-of::tex
% Description: TeX, LaTeX and DVI

% Tag: made-of::vrml
% Description: VRML, Virtual Reality Markup Language

% Tag: made-of::xml
% Description: XML


%%******************************************************************************
%% Facet: interface
%% Description: User Interface
%%  What kind of user interface the package provides

% Tag: interface::commandline
% Description: Command Line
tag({application, _, {type, esc}}) -> 'interface::commandline' ;

% Tag: interface::daemon
% Description: Daemon
%  Runs in background, only a control interface is provided, usually on
%  commandline.
tag({application, _, {type, otp}}) -> 'interface::daemon' ;
tag({application, _, {type, app}}) -> 'interface::daemon' ;

% Tag: interface::graphical
% Description: Graphical User Interface
%  Packages that provide the user with a 2D graphical user interface.

% Tag: interface::shell
% Description: Command Shell

% Tag: interface::text-mode
% Description: Text-based Interactive

% Tag: interface::web
% Description: World Wide Web

%%******************************************************************************
%% Facet: works-with
%% Description: Works with
%%  What kind of data (or even processes, or people) the package can work with

% Tag: works-with::archive
% Description: Archive

% Tag: works-with::db
% Description: Databases

% Tag: works-with::dictionary
% Description: Dictionaries

% Tag: works-with::file
% Description: Files

% Tag: works-with::graphs
% Description: Trees and Graphs

% Tag: works-with::logfile
% Description: System Logs

% Tag: works-with::network-traffic
% Description: Network Traffic
%  Routers, shapers, sniffers, firewalls and other tools
%  that work with a stream of network packets.

%%******************************************************************************
%% Facet: scope
%% Description: Scope
%%  Characterization by scale of coverage 

% Tag: scope::utility
% Description: Utility
%  A narrow-scoped program for particular use case or few use cases. It
%  only does something 10-20% of users in the field will need. Often has
%  functionality missing from related applications.
%  NOTE : Escripts
tag({export, _, {_, main, 1}}) -> 'scope::utility' ;

% Tag: scope::application
% Description: Application
%  Broad-scoped program for general use. It probably has functionality
%  for 80-90% of use cases. The pieces that remain are usually to be
%  found as utilities.
%  NOTE : OTP application
tag({export, _, {application, start, 2}}) -> 'scope::application' ;

% Tag: scope::suite
% Description: Suite
%  Comprehensive suite of applications and utilities on the scale of
%  desktop environment or base operating system.
%  NOTE : Several OTP applications working together
tag({call, _, {application, load, 1}}) -> 'scope::suite' ;
tag({call, _, {application, start, 1}}) -> 'scope::suite' ;


%%******************************************************************************
%% Facet: role
%% Description: Role
%%  Role performed by the package

% Tag: role::devel-lib
% Description: Development Library
%  Library and header files used in software development or building.
% NOTE : .hrl files only

% Tag: role::plugin
% Description: Plugin
%  Add-on, pluggable program fragments enhancing functionality
%  of some program or system.
% NOTE : erlang.mk/rebar plugin

% Tag: role::program
% Description: Program
%  Executable computer program.
% NOTE : OTP and non OTP application

% Tag: role::shared-lib
% Description: Shared Library
%  Shared libraries used by one or more programs.
% NOTE : Erlang module

%%******************************************************************************
%% Facet: special
%% Description: Service tags
%%  Group of special tags

% Tag: special::unreviewed
% Description: Needs review

%%******************************************************************************
%% Facet: protocol
%% Description: Network Protocol
%%  Which network protocols the package can understand

% Tag: protocol::corba
% Description: CORBA
%  Common Object Request Broker Architecture, a standard for interoperability
%  between programs written in different languages and running on different
%  hardware platforms. CORBA includes a client-server network protocol for
%  distributed computing.
tag({call, _, {corba, _, _}}) -> 'protocol::corba' ;
tag({call, _, {corba_object, _, _}}) -> 'protocol::corba' ;

% Tag: protocol::db:mysql
% Description: MySQL
%  Protocol for accessing MySQL database server.
tag({call, _, {emysql, _, _}})      -> 'protocol::db:mysql';

% Tag: protocol::db:psql
% Description: PostgreSQL
%  Protocol for accessing PostgreSQL database server.
tag({call, _, {epgsql, _, _}})      -> 'protocol::db:psql';

% Tag: protocol::ftp
% Description: FTP
%  File Transfer Protocol, a protocol for exchanging and manipulation files over
%  networks and extensively used on the Internet.
tag({call, _, {ftp, _, _}}) -> 'protocol::ftp' ;

% Tag: protocol::http
% Description: HTTP
%  HyperText Transfer Protocol, one of the most important protocols for the
%  World Wide Web.
tag({call, _, {httpc, _, _}})            -> 'protocol::http' ;
tag({call, _, {httpd, _, _}})            -> 'protocol::http' ;
tag({call, _, {http_uri, _, _}})         -> 'protocol::http' ;
tag({call, _, {httpd_util, _, _}})       -> 'protocol::http' ;
tag({call, _, {httpd_socket, _, _}})     -> 'protocol::http' ;
tag({call, _, {httpd_custom_api, _, _}}) -> 'protocol::http' ;

% Tag: protocol::ipv6
% Description: IPv6
%  Internet Protocol (v6), the next-generation Internet protocol, which overcomes
%  the restrictions of IP (v4), like shortage of IP addresses, and is supposed to
%  form the new basis of the Internet in the future, replacing IP (v4).
tag({call, _, {inet, parse_ipv6_address, 1}}) -> 'protocol::ipv6' ;
tag({call, _, {inet, parse_ipv6strict_address, 1}}) -> 'protocol::ipv6' ;

% Tag: protocol::ip
% Description: IP
%  Internet Protocol (v4), a core protocol of the Internet protocol suite and
%  the very basis of the Internet.
tag({call, _, {inet, _, _}}) -> 'protocol::ip' ;

% Tag: protocol::ldap
% Description: LDAP
%  Lightweight Directory Access Protocol
tag({call, _, {eldap, _, _}})         -> 'protocol::ldap' ;

% Tag: protocol::sftp
% Description: SFTP
%  SSH File Transfer Protocol, a protocol for secure, encrypting file exchange
%  and manipulation over insecure networks, using the SSH protocol.
tag({call, _, {ssh_sftp, _, _}})         -> 'protocol::sftp' ;
tag({call, _, {ssh_sftpd, _, _}})        -> 'protocol::sftp' ;

% Tag: protocol::snmp
% Description: SNMP
%  Simple Network Management Protocol, a member of the Internet protocol suite
%  and used for monitoring or configuring network devices.
tag({call, _, {snmp, _, _}})         -> 'protocol::snmp' ;

% Tag: protocol::ssh
% Description: SSH
%  Secure Shell, a protocol for secure, encrypted network connections. SSH can
%  be used to execute programs on a remote host with an SSH server over otherwise
%  insecure protocols through an SSH channel. The main use is, as the name
%  suggest, to provide encrypted login and shell access on remote servers.
tag({call, _, {ssh, _, _}})                -> 'protocol::ssh' ;
tag({call, _, {ssh_channel, _, _}})        -> 'protocol::ssh' ;
tag({call, _, {ssh_connection, _, _}})     -> 'protocol::ssh' ;
tag({call, _, {ssh_client_key_api, _, _}}) -> 'protocol::ssh' ;
tag({call, _, {ssh_server_key_api, _, _}}) -> 'protocol::ssh' ;
tag({call, _, {ct_ssh, _, _}})             -> 'protocol::ssh' ;

% Tag: protocol::ssl
% Description: SSL/TLS
%  Secure Socket Layer/Transport Layer Security, a protocol that provides
%  secure encrypted communication on the Internet. It is used to authenticate
%  the identity of a service provider (such as a Internet banking server) and
%  to secure the communications channel.
tag({call, _, {ssl, _, _}})             -> 'protocol::ssl' ;

% Tag: protocol::telnet
% Description: Telnet
%  TELecommunication NETwork, a mostly superseded protocol for remote logins.
tag({call, _, {ct_telnet, _, _}})             -> 'protocol::telnet' ;
tag({call, _, {unix_telnet, _, _}})           -> 'protocol::telnet' ;

% Tag: protocol::tcp
% Description: TCP
%  Transport Control Protocol, a core protocol of the Internet protocol suite
%  and used for data transport.
tag({call, _, {gen_tcp, _, _}})       -> 'protocol::tcp' ;
tag({call, _, {megaco_tcp, _, _}})    -> 'protocol::tcp' ;
tag({call, _, {diameter_tcp, _, _}})  -> 'protocol::tcp' ;

% Tag: protocol::tftp
% Description: TFTP
%  Trivial File Transfer Protocol, a simple file transfer protocol.  TFTP allows
%  a client to get or put a file onto a remote host.  One of its primary uses is
%  the network booting of diskless nodes on a Local Area Network.  It is designed
%  to be easy to implement so it fits on ROM.
tag({call, _, {tftp, _, _}})             -> 'protocol::tftp' ;

% Tag: protocol::udp
% Description: UDP
%  User Datagram Protocol, a core protocol of the Internet protocol suite
%  and used for data transport.
tag({call, _, {gen_udp, _, _}})             -> 'protocol::udp' ;
tag({call, _, {megaco_udp, _, _}})          -> 'protocol::udp' ;


%%******************************************************************************
%% Facet: uitoolkit
%% Description: Interface Toolkit
%%  Which interface toolkit the package provides

% Tag: uitoolkit::wxwidgets
% Description: wxWidgets
tag({call, _, {wx, _, _}})          -> 'uitoolkit::wxwidgets' ;


%%******************************************************************************
%% Facet: web
%% Description: World Wide Web
%%  What kind of tools for the World Wide Web the package provides

% Tag: web::application
% Description: Application
% NOTE : application depending of usual Erlang web-servers
tag({call, _, {inets, _, _}})      -> 'web::application';
tag({call, _, {cowboy, _, _}})     -> 'web::application';
tag({call, _, {mochiweb, _, _}})   -> 'web::application';
tag({call, _, {webmachine, _, _}}) -> 'web::application';
tag({call, _, {yaws, _, _}})       -> 'web::application';

% Tag: web::server
% Description: Server
% NOTE : already knowned Erlang webservers
tag({application, cowboy, _})     -> 'web::server' ;
tag({application, mochiweb, _})   -> 'web::server' ;
tag({application, webmachine, _}) -> 'web::server' ;
tag({application, yaws, _})       -> 'web::server' ;

%%******************************************************************************
%% Facet: network
%% Description: Networking
%%  Role performed concerning computer networks

% Tag: network::client
% Description: Client

% Tag: network::server
% Description: Server

%%******************************************************************************
tag({_, _, _}) -> [].

-define(Facets, ['devel', 'made-of', 'interface', 'implemented-in', 'works-with',
                 'work-with-format', 'scope', 'role', 'special', 'protocol',
                 'uitoolkit', 'web', 'network'
                ]).

-define(Tags, ?Devel ++ ?Made_of ++ ?Interface ++ ?Implemented_in ++ ?Works_with
                ++ ?Works_with_format ++ ?Scope ++ ?Role ++ ?Special ++ ?Protocol
                ++ ?Uitoolkit ++ ?Web ++ ?Network ).



