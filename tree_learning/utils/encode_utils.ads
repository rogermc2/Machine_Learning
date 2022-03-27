
with Ada.Containers.Ordered_Sets;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Classifier_Types; use Classifier_Types;
with ML_Types;

package Encode_Utils is

    package UB_String_Sets is new
      Ada.Containers.Ordered_Sets (Unbounded_String);
    Encode_Error : exception;

    function Encode (Values : ML_Types.Value_Data_List)
                     return ML_Types.Value_Data_List;
    function Encode (Values        : ML_Types.Value_Data_List;
                     Uniques       : ML_Types.Value_Data_List;
                     Check_Unknown : Boolean := True)
                     return Natural_List;
    function Map_To_Integer (Values  : ML_Types.Value_Data_List;
                             Uniques : ML_Types.Value_Data_List)
                             return Natural_List;
    function Unique (Values : Natural_List) return Natural_List;
    function Unique (Values : ML_Types.Value_Data_List)
                     return ML_Types.Value_Data_List;
    function Unique (Values  : ML_Types.Value_Data_List;
                     Inverse : out Natural_List)
                     return ML_Types.Value_Data_List;

end Encode_Utils;