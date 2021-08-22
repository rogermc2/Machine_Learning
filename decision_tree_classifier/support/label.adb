
--  Adapted from scikit-learn/scikit-learn.git sklearn/preprocessing/_label.py

--  with Ada.Text_IO; use Ada.Text_IO;

with Classifier_Utilities;
with Encode_Utils;

package body Label is

    --  -------------------------------------------------------------------------
    --  A Fit function adjusts weights according to data values so that better accuracy can be achieved.
    --  Fit fits label encoder
    function Fit (Encoder : in out Label_Encoder;
                  Y : ML_Types.Value_Data_List) return Label_Encoder is
    begin
        Encoder.Classes := Encode_Utils.Unique (Y);
        return Encoder;
    end Fit;

    --  -------------------------------------------------------------------------
    --  Fit_Transform fits label encoder and returns encoded labels
    --  Balanced class weights should be given by
    --  n_samples / (n_classes * np.bincount(y))
    function Fit_Transform (Self : in out Label_Encoder;
                            Y    : ML_Types.Value_Data_List)
                           return Natural_List is
        Encoded_Labels : Natural_List;
    begin
        Self.Classes := Encode_Utils.Unique (Y, Encoded_Labels, True);
        Classifier_Utilities.Print_Value_List
          ("Label.Fit_Transform Classes", Self.Classes);
        Classifier_Utilities.Print_Natural_List
          ("Label.Fit_Transform Labels", Encoded_Labels);
        return Encoded_Labels;

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
    --  Transform returns labels as normalized encodings
    function Transform (Self : in out Label_Encoder;
                        Y    : ML_Types.Value_Data_List)
                       return Natural_List is
        Labels  : Natural_List := Natural_Package.Empty_Vector;
        Uniques : ML_Types.Value_Data_List;
    begin
        pragma Warnings (Off, Uniques);
        if not Y.Is_Empty then
            Uniques := Encode_Utils.Encode (Y, Labels, Do_Encode => True);
            --        Self.Classes := Labels;
        end if;
        return Labels;
    end Transform;

    --  -------------------------------------------------------------------------

end Label;
