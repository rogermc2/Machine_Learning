
with Classifier_Utilities;

package body Label is

    function Encode_Check_Unknown
      (Values : ML_Types.Value_Data_List; Uniques : ML_Types.Value_Data_List)
       return ML_Types.Value_Data_List;

    --  -------------------------------------------------------------------------
    --  Values : values to factorize or encode.
    --  Uniques : If not empty, uniques are not determined from passed Values
    --  This can be because the user specified categories or because they
    --  already have been determined in fit.
    --  Encode : if True, encode Values into integer codes based on Uniques.
    --  Check_Unknown : if True check Values for values that are not in Uniques
    --  and raise an error.
    function Encode (Values        : ML_Types.Value_Data_List;
                     Uniques       : in out ML_Types.Value_Data_List;
                     Do_Encode     : Boolean := False;
                     Check_Unknown : Boolean := True) return Natural_List is
        Inverse       : Natural_List :=
                          Classifier_Types.Natural_Package.Empty_Vector;
        Sorted_Values : ML_Types.Value_Data_List := Values;
        Diff          : ML_Types.Value_Data_List;
        Encoded       : Natural_List;
    begin
        Encoded.Clear;
        ML_Types.Value_Data_Sorting.Sort (Sorted_Values);
        if Uniques.Is_Empty then
            if Do_Encode then
                Uniques := Classifier_Utilities.Unique_Values
                  (Sorted_Values, Encoded, Return_Inverse => True);
            else
                Uniques := Classifier_Utilities.Unique_Values (Sorted_Values, Inverse);
            end if;
        end if;

        if Do_Encode then
            if Check_Unknown then
                Diff := Encode_Check_Unknown (Values, Uniques);
                if not Diff.Is_Empty then
                    Classifier_Utilities.Print_Value_List
                      ("Unique list", Uniques);
                    Classifier_Utilities.Print_Value_List
                      ("Unseen labels", Diff);
                    raise Label_Error with
                      "Label.Encode Values contains previously unseen labels.";
                end if;
            end if;

            for index in Uniques.First_Index .. Uniques.Last_Index loop
                Encoded.Append (index);
            end loop;
        end if;

        return Encoded;
    end Encode;

    --  -------------------------------------------------------------------------

    function Encode_Check_Unknown
      (Values : ML_Types.Value_Data_List; Uniques : ML_Types.Value_Data_List)
       return ML_Types.Value_Data_List is
        use ML_Types;
        No_Inverse  : Natural_List :=
                        Classifier_Types.Natural_Package.Empty_Vector;
        Unique_Vals : constant Value_Data_List :=
                        Classifier_Utilities.Unique_Values (Values, No_Inverse);
        aVal        : Value_Record;
        Diff        : Value_Data_List;
    begin
        for index in Unique_Vals.First_Index .. Unique_Vals.Last_Index loop
            aVal := Unique_Vals.Element (index);
            if not Uniques.Contains (aVal) then
                Diff.Append (aVal);
            end if;
        end loop;

        return Diff;
    end Encode_Check_Unknown;

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
    --  Balanced class weights should be given by
    --  n_samples / (n_classes * np.bincount(y))
    function Fit_Transform (Self : in out Label_Encoder;
                            Y    : ML_Types.Value_Data_List)
                            return Natural_List is
        Labels  : Natural_List;
        Uniques : ML_Types.Value_Data_List;
    begin
        Uniques.Clear;
        Labels := Encode (Y, Uniques, True);
        Classifier_Utilities.Print_Natural_List
          ("Label.Fit_Transform Labels", Labels);
        Classifier_Utilities.Print_Value_List
          ("Label.Fit_Transform Uniques", Uniques);
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
    --  Balanced class weights should be given by
    --  n_samples / (n_classes * np.bincount(y))
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
