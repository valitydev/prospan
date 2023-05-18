-module(prospan_uploader).

-export([upload/2]).

-spec upload(binary(), map()) -> ok.
upload(Bin, #{base_uri := BaseUri} = _Config) when is_binary(Bin) ->
    Request = {BaseUri ++ "/api/v2/spans", [], "application/json", Bin},
    case httpc:request(post, Request, [], []) of
        {ok, {{_Protocol, 202, _StatusText}, _Headers, _Body}} -> ok;
        {ok, NotExpected} -> error(NotExpected);
        {error, Reason} -> error(Reason)
    end.
