%% -*- coding: utf-8 -*-
%% Automatically generated, do not edit
%% Generated by gpb_compile version 4.20.0

-ifndef(pb_Login).
-define(pb_Login, true).

-define(pb_Login_gpb_version, "4.20.0").


-ifndef('LOGINREQ_PB_H').
-define('LOGINREQ_PB_H', true).
-record('LoginReq',
        {account                :: unicode:chardata() | undefined, % = 1, required
         loginPass              :: unicode:chardata() | undefined, % = 2, required
         loginType              :: integer() | undefined, % = 3, required, 32 bits
         version                :: unicode:chardata() | undefined, % = 4, optional
         jsVersion              :: unicode:chardata() | undefined, % = 5, optional
         channel                :: unicode:chardata() | undefined % = 6, optional
        }).
-endif.

-ifndef('LOGINRESP_PB_H').
-define('LOGINRESP_PB_H', true).
-record('LoginResp',
        {uid                    :: unicode:chardata() | undefined, % = 1, required
         token                  :: unicode:chardata() | undefined, % = 2, required
         expire_in              :: non_neg_integer() | undefined, % = 3, optional, 32 bits
         mission                :: integer() | undefined, % = 4, optional, 32 bits
         max_score              :: integer() | undefined, % = 5, optional, 32 bits
         num                    :: integer() | undefined % = 6, optional, 32 bits
        }).
-endif.

-endif.
