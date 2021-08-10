
package body Label is

    --  -------------------------------------------------------------------------

    function Encode (Values     : ML_Types.Value_Data_List;
                     Uniques    : in out ML_Types.Value_Data_List;
                     Do_Encode  : Boolean := False) return Natural_List is
        Sorted_Values : ML_Types.Value_Data_List := Values;
        Encoded       : Natural_List;
    begin
        if Uniques.Is_Empty then
            ML_Types.Value_Data_Sorting.Sort (Sorted_Values);
            Uniques := Sorted_Values;
        end if;
        if Do_Encode then
            for index in Uniques.First_Index .. Uniques.Last_Index loop
                Encoded.Append (index);
            end loop;
        end if;

        return Encoded;
    end Encode;

    --  -------------------------------------------------------------------------
    --  Fit fits label encoder
    function Fit (Y : ML_Types.Value_Data_List) return Label_Encoder is
        Encoder_New : Label_Encoder;
        Uniques     :  ML_Types.Value_Data_List;
    begin
        Encoder_New.Classes := Encode (Y, Uniques);
        return Encoder_New;
    end Fit;

    --  -------------------------------------------------------------------------
    --  Fit_Transform fits label encoder and returns encoded labels
    function Fit_Transform (Self : in out Label_Encoder;
                            Y    : ML_Types.Value_Data_List)
                           return Natural_List is
        Labels  : Natural_List;
        Uniques : ML_Types.Value_Data_List;
    begin
        Labels := Encode (Y, Uniques, True);
        Self.Classes := Labels;
        return Labels;
    end Fit_Transform;

    --  -------------------------------------------------------------------------

--      function Inverse_Transform (Self : in out Label_Encoder;
--                                  Y    : ML_Types.Value_Data_List)
--                                 return Natural_List is
--          Labels  : Natural_List;
--          Uniques : ML_Types.Value_Data_List;
--      begin
--          Labels := Encode (Y, Uniques, Do_Encode => True);
--          Self.Classes := Labels;
--          return Labels;
--      end Inverse_Transform;

    --  -------------------------------------------------------------------------

    function Transform (Self : in out Label_Encoder;
                        Y    : ML_Types.Value_Data_List)
                       return Natural_List is
        Labels  : Natural_List;
        Uniques : ML_Types.Value_Data_List;
    begin
        Labels := Encode (Y, Uniques, Do_Encode => True);
        Self.Classes := Labels;
        return Labels;
    end Transform;

    --  -------------------------------------------------------------------------

end Label;
