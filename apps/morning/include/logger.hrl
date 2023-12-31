% /*
%  * @Author: mrchenxinbo 
%  * @Date: 2023-12-11 16:12:21 
%  * @Last Modified by:   mrchenxinbo 
%  * @Last Modified time: 2023-12-11 16:12:21 
%  */


-ifndef(__LOGGER_HRL__).

-define(__LOGGER_HRL__, 1).
-define(PRINT(Format, Args), io:format(Format, Args)).
-compile([{parse_transform, lager_transform}]).

-ifdef(TEST).
-ifdef(OTP18).

-define(DEBUG(Format, Args), ok).

-else.

-define(DEBUG(Format, Args), lager:debug(Format, Args)).

-endif.

-else.

-define(DEBUG(Format, Args), lager:debug(Format, Args)).

-endif.

-define(DEBUG(Meta, Format, Args), lager:debug(Meta, Format, Args)).
-define(INFO_MSG(Format, Args), lager:info(Format, Args)).
-define(INFO_MSG(Placeholder, Format, Args), lager:info(Placeholder, Format, Args)).
-define(WARNING_MSG(Format, Args), lager:warning(Format, Args)).
-define(ERROR_MSG(Format, Args), lager:error(Format, Args)).
-define(ERROR_MSG(Meta, Format, Args), lager:error(Meta, Format, Args)).
-define(CRITICAL_MSG(Format, Args), lager:critical(Format, Args)).

-endif.
