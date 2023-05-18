-module(prospan_conf).

-export([alias_rpc_service/1]).

-spec alias_rpc_service(binary()) -> binary().
alias_rpc_service(<<"InspectorProxy">>) ->
    <<"fraud-buster">>;
alias_rpc_service(<<"Accounter">>) ->
    <<"shumway">>;
alias_rpc_service(<<"FaultDetector">>) ->
    <<"fault-detector">>;
alias_rpc_service(<<"Arbiter">>) ->
    <<"bouncer">>;
alias_rpc_service(<<"TokenAuthenticator">>) ->
    <<"token-authenticator">>;
alias_rpc_service(<<"Limiter">>) ->
    <<"limiter">>;
alias_rpc_service(<<"Automaton">>) ->
    <<"machinegun-ha">>;
alias_rpc_service(<<"Invoicing">>) ->
    <<"hellgate">>;
alias_rpc_service(<<"Generator">>) ->
    <<"bender">>;
alias_rpc_service(<<"Bender">>) ->
    <<"bender">>;
alias_rpc_service(<<"Management">>) ->
    <<"party-management">>;
alias_rpc_service(<<"PartyManagement">>) ->
    <<"party-management">>;
alias_rpc_service(Service) ->
    Service.
