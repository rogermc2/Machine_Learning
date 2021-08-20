
with Classifier_Types; use Classifier_Types;
with ML_Types;

package Encode_Utils is

    function Unique (Values : ML_Types.Value_Data_List;
                     Inverse        : out Natural_List;
                     Return_Inverse : Boolean := False)
                     return ML_Types.Value_Data_List;

end Encode_Utils;
