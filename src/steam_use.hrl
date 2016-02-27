%%% File: steam_use.hrl
%%% @copyright Eric Pailleau <steam@crownedgrouse.com>
%%% @licence   https://github.com/crownedgrouse/steam/blob/master/LICENCE
%%% @doc 
%%% Steam database for facet 'use'
%%% @end 


%%******************************************************************************
%% Facet: use
%% Description: Purpose
%%  The general purpose of the software
%% 
%% NOTE : Please keep alphabetical order for applications per tag !

% Tag: use::analysing
% Description: Analysing
%  Software for turning data into knowledge.
use(geas)  -> 'use::analysing' ;
use(steam) -> 'use::analysing' ;

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
use(swab) -> 'use::converting' ;

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
use(debris) -> 'use::organizing' ;
use(debbie) -> 'use::organizing' ;
use(debut)  -> 'use::organizing' ;
use(edgar)  -> 'use::organizing' ;

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

use(_) -> [].


