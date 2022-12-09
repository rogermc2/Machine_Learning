
--  Adapted from scikit-learn/scikit-learn.git sklearn/preprocessing/_label.py
--  A LabelEncoder encodes labels with a value between 0 and n_classes-1 where
--  n is the number of distinct labels. If a label repeats it assigns the same
--  value to as assigned earlier. The categorical values have been converted
--   into numeric values.
--    Examples
--      --------
--      `LabelEncoder` can be used to normalize labels:
--      >>> from sklearn import preprocessing
--      >>> le = preprocessing.LabelEncoder()
--      >>> le.fit([1, 2, 2, 6])
--      LabelEncoder()
--      >>> le.classes_
--      array([1, 2, 6])
--      >>> le.transform([1, 1, 2, 6])
--      array([0, 0, 1, 2]...)
--      >>> le.inverse_transform([0, 0, 1, 2])
--      array([1, 1, 2, 6])

--      It can also be used to transform non-numerical labels (as long as they are
--      hashable and comparable) to numerical labels.
--      >>> le = preprocessing.LabelEncoder()
--      >>> le.fit(["paris", "paris", "tokyo", "amsterdam"])
--      LabelEncoder()
--      >>> list(le.classes_)
--      ['amsterdam', 'paris', 'tokyo']
--      >>> le.transform(["tokyo", "tokyo", "paris"])
--      array([2, 2, 1]...)
--      >>> list(le.inverse_transform([2, 2, 1]))
--      ['tokyo', 'tokyo', 'paris']

with Ada.Assertions; use Ada.Assertions;
--  with Ada.Text_IO; use Ada.Text_IO;

with Classifier_Utilities;
with Encode_Utils;

package body Label is

    --  -------------------------------------------------------------------------
    --  Fit fits label encoder
    procedure Fit (Encoder : in out Label_Encoder;
                   Y       : ML_Types.Value_Data_List) is
    begin
        if Encoder.Encoder_Kind = Class_Unique then
            Encoder.Uniques := Encode_Utils.Unique (Y);
        else
            raise Label_Error with
              "Label.Fit called with label encoder instead of unique encode";
        end if;
    end Fit;

    --  -------------------------------------------------------------------------
    --  Fit_Transform fits label encoder and returns encoded labels
    --  Balanced class weights should be given by
    --  n_samples / (n_classes * np.bincount(y))
    function Fit_Transform (Encoder : in out Label_Encoder;
                            Y    : ML_Types.Value_Data_List)
                           return NL_Types.Natural_List is
        Encoded_Labels : NL_Types.Natural_List;
    begin
        if Encoder.Encoder_Kind = Class_Unique then
            Encoder.Uniques := Encode_Utils.Unique (Y, Encoded_Labels);
        else
            raise Label_Error with
              "Label.Fit_Transform called with label encoder instead of unique encoder";
        end if;

        return Encoded_Labels;

    end Fit_Transform;

    --  -------------------------------------------------------------------------
    --   Inverse_Transform transforms labels back to original encoding
    function Inverse_Transform (Self    : in out Label_Encoder;
                                Labels  : NL_Types.Natural_List)
                               return ML_Types.Value_Data_List is
        aRange  : NL_Types.Natural_List;
        Diff    : NL_Types.Natural_List;
        Result  : ML_Types.Value_Data_List :=
                    ML_Types.Value_Data_Package.Empty_Vector;
    begin
        if not Labels.Is_Empty then
            for index in 1 .. Positive (Self.Uniques.Length) loop
                aRange.Append (index);
            end loop;

            Diff := Classifier_Utilities.Set_Diff (Labels, aRange);
            Assert (Diff.Is_Empty,
                    "Label.Inverse_Transform Labels vector contains " &
                      "previously unseen labels.");

            for index in 1 .. Positive (Labels.Length) loop
                Result.Append (Self.Uniques.Element (Labels.Element (index)));
            end loop;
        end if;
        return Result;

    end Inverse_Transform;

    --  -------------------------------------------------------------------------
    --  Transform returns labels as normalized encodings
    function Transform (Self : in out Label_Encoder;
                        Y    : ML_Types.Value_Data_List)
                       return NL_Types.Natural_List is
        Labels  : NL_Types.Natural_List;
    begin
        if not Y.Is_Empty then
            if Self.Encoder_Kind = Class_Unique then
                Labels := Encode_Utils.Encode (Y, Self.Uniques);
            else
                raise Label_Error with
                  "Label.Transform called with invalid encoder type.";
            end if;
        end if;
        return Labels;

    end Transform;

    --  -------------------------------------------------------------------------

end Label;
