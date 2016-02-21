%%% File: steam_db.hrl
%%% @copyright Eric Pailleau <steam@crownedgrouse.com>
%%% @licence   https://github.com/crownedgrouse/steam/blob/master/LICENCE
%%% @doc 
%%% Steam database 
%%% @end 


%%******************************************************************************
%% Facet: devel
%% Description: Software Development
%%  How the package is related to the field of software development
-define(Devel, ['devel::lang:erlang']).

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
-define(Made_of, []).

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
-define(Interface, []).

% Tag: interface::commandline
% Description: Command Line

% Tag: interface::daemon
% Description: Daemon
%  Runs in background, only a control interface is provided, usually on
%  commandline.

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
%% Facet: implemented-in
%% Description: Implemented in
%%  What language the software is implemented in
-define(Implemented_in, []).

% Tag: implemented-in::c
% Description: C

% Tag: implemented-in::c++
% Description: C++

% Tag: implemented-in::erlang
% Description: Erlang

% Tag: implemented-in::java
% Description: Java

%%******************************************************************************
%% Facet: works-with
%% Description: Works with
%%  What kind of data (or even processes, or people) the package can work with
-define(Works_with, []).

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
%% Facet: works-with-format
%% Description: Supports Format
%%  Which data formats are supported by the package
-define(Works_with_format,[]).

% Tag: works-with-format::json
% Description: JSON
%  JavaScript Object Notation

% Tag: works-with-format::xml
% Description: XML

% Tag: works-with-format::xml:xslt
% Description: XSL Transformations (XSLT)

% Tag: works-with-format::zip
% Description: Zip Archives

%%******************************************************************************
%% Facet: scope
%% Description: Scope
%%  Characterization by scale of coverage 
-define(Scope, []).

% Tag: scope::utility
% Description: Utility
%  A narrow-scoped program for particular use case or few use cases. It
%  only does something 10-20% of users in the field will need. Often has
%  functionality missing from related applications.
%  NOTE : Escripts

% Tag: scope::application
% Description: Application
%  Broad-scoped program for general use. It probably has functionality
%  for 80-90% of use cases. The pieces that remain are usually to be
%  found as utilities.
%  NOTE : OTP application

% Tag: scope::suite
% Description: Suite
%  Comprehensive suite of applications and utilities on the scale of
%  desktop environment or base operating system.
%  NOTE : Several OTP applications working together


%%******************************************************************************
%% Facet: role
%% Description: Role
%%  Role performed by the package
-define(Role, []).

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
-define(Special, []).

% Tag: special::unreviewed
% Description: Needs review

%%******************************************************************************
%% Facet: protocol
%% Description: Network Protocol
%%  Which network protocols the package can understand
-define(Protocol, []).

% Tag: protocol::corba
% Description: CORBA
%  Common Object Request Broker Architecture, a standard for interoperability
%  between programs written in different languages and running on different
%  hardware platforms. CORBA includes a client-server network protocol for
%  distributed computing.

% Tag: protocol::db:mysql
% Description: MySQL
%  Protocol for accessing MySQL database server.

% Tag: protocol::db:psql
% Description: PostgreSQL
%  Protocol for accessing PostgreSQL database server.

% Tag: protocol::ftp
% Description: FTP
%  File Transfer Protocol, a protocol for exchanging and manipulation files over
%  networks and extensively used on the Internet.

% Tag: protocol::http
% Description: HTTP
%  HyperText Transfer Protocol, one of the most important protocols for the
%  World Wide Web.

% Tag: protocol::ip
% Description: IP
%  Internet Protocol (v4), a core protocol of the Internet protocol suite and
%  the very basis of the Internet.

% Tag: protocol::ipv6
% Description: IPv6
%  Internet Protocol (v6), the next-generation Internet protocol, which overcomes
%  the restrictions of IP (v4), like shortage of IP addresses, and is supposed to
%  form the new basis of the Internet in the future, replacing IP (v4).

% Tag: protocol::ldap
% Description: LDAP
%  Lightweight Directory Access Protocol

% Tag: protocol::sftp
% Description: SFTP
%  SSH File Transfer Protocol, a protocol for secure, encrypting file exchange
%  and manipulation over insecure networks, using the SSH protocol.

% Tag: protocol::snmp
% Description: SNMP
%  Simple Network Management Protocol, a member of the Internet protocol suite
%  and used for monitoring or configuring network devices.

% Tag: protocol::ssh
% Description: SSH
%  Secure Shell, a protocol for secure, encrypted network connections. SSH can
%  be used to execute programs on a remote host with an SSH server over otherwise
%  insecure protocols through an SSH channel. The main use is, as the name
%  suggest, to provide encrypted login and shell access on remote servers.

% Tag: protocol::ssl
% Description: SSL/TLS
%  Secure Socket Layer/Transport Layer Security, a protocol that provides
%  secure encrypted communication on the Internet. It is used to authenticate
%  the identity of a service provider (such as a Internet banking server) and
%  to secure the communications channel.

% Tag: protocol::telnet
% Description: Telnet
%  TELecommunication NETwork, a mostly superseded protocol for remote logins.

% Tag: protocol::tcp
% Description: TCP
%  Transport Control Protocol, a core protocol of the Internet protocol suite
%  and used for data transport.

% Tag: protocol::tftp
% Description: TFTP
%  Trivial File Transfer Protocol, a simple file transfer protocol.  TFTP allows
%  a client to get or put a file onto a remote host.  One of its primary uses is
%  the network booting of diskless nodes on a Local Area Network.  It is designed
%  to be easy to implement so it fits on ROM.

% Tag: protocol::udp
% Description: UDP
%  User Datagram Protocol, a core protocol of the Internet protocol suite
%  and used for data transport.


%%******************************************************************************
%% Facet: uitoolkit
%% Description: Interface Toolkit
%%  Which interface toolkit the package provides
-define(Uitoolkit,[]).

% Tag: uitoolkit::wxwidgets
% Description: wxWidgets

%%******************************************************************************
%% Facet: use
%% Description: Purpose
%%  The general purpose of the software
%% NOTE : probably not automatisable. Keep for memory for now.
%-define('use',[]).

% Tag: use::analysing
% Description: Analysing
%  Software for turning data into knowledge.

% Tag: use::browsing
% Description: Browsing

% Tag: use::calculating
% Description: Calculating

% Tag: use::chatting
% Description: Chatting

% Tag: use::checking
% Description: Checking
%  All sorts of checking, checking a filesystem for validity, checking
%  a document for incorrectly spelled words, checking a network for
%  routing problems. Verifying.

% Tag: use::comparing
% Description: Comparing
%  To find what relates or differs in two or more objects.

% Tag: use::compressing
% Description: Compressing

% Tag: use::configuring
% Description: Configuration

% Tag: use::converting
% Description: Data Conversion

% Tag: use::dialing
% Description: Dialup Access

% Tag: use::downloading
% Description: Downloading

% Tag: use::driver
% Description: Hardware Driver

% Tag: use::editing
% Description: Editing

% Tag: use::entertaining
% Description: Entertaining

% Tag: use::filtering
% Description: Filtering

% Tag: use::gameplaying
% Description: Game Playing

% Tag: use::learning
% Description: Learning

% Tag: use::login
% Description: Login

% Tag: use::measuring
% Description: Measuring

% Tag: use::monitor
% Description: Monitoring

% Tag: use::organizing
% Description: Data Organisation

% Tag: use::playing
% Description: Playing Media

% Tag: use::printing
% Description: Printing

% Tag: use::proxying
% Description: Proxying

% Tag: use::routing
% Description: Routing

% Tag: use::searching
% Description: Searching

% Tag: use::scanning
% Description: Scanning

% Tag: use::simulating
% Description: Simulating

% Tag: use::storing
% Description: Storing

% Tag: use::synchronizing
% Description: Synchronisation

% Tag: use::timekeeping
% Description: Time and Clock

% Tag: use::transmission
% Description: Transmission

% Tag: use::typesetting
% Description: Typesetting

% Tag: use::viewing
% Description: Data Visualization

% Tag: use::text-formatting
% Description: Text Formatting


%%******************************************************************************
%% Facet: web
%% Description: World Wide Web
%%  What kind of tools for the World Wide Web the package provides
-define(Web,[]).

% Tag: web::application
% Description: Application
% NOTE : application depending of usual Erlang web-servers

% Tag: web::server
% Description: Server
% NOTE : already knowned Erlang webservers

%%******************************************************************************
%% Facet: network
%% Description: Networking
%%  Role performed concerning computer networks
-define(Network,[]).

% Tag: network::client
% Description: Client

% Tag: network::server
% Description: Server

%%******************************************************************************
-define(Facets, ['devel', 'made-of', 'interface', 'implemented-in', 'works-with',
                 'work-with-format', 'scope', 'role', 'special', 'protocol',
                 'uitoolkit', 'web', 'network'
                ]).

-define(Tags, ?Devel ++ ?Made_of ++ ?Interface ++ ?Implemented_in ++ ?Works_with
                ++ ?Works_with_format ++ ?Scope ++ ?Role ++ ?Special ++ ?Protocol
                ++ ?Uitoolkit ++ ?Web ++ ?Network ).



