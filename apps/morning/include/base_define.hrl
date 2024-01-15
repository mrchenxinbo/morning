% /*
%  * @Author: mrchenxinbo 
%  * @Date: 2024-01-15 19:07:14 
%  * @Last Modified by:   mrchenxinbo 
%  * @Last Modified time: 2024-01-15 19:07:14 
%  */

-define(IF(C, T, F), (case (C) of true -> (T); false -> (F) end)).
-define(IFDO(C, T),(case (C) of true -> (T); false -> nothing end)).