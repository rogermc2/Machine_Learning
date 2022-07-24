
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
with Ada.Text_IO; use Ada.Text_IO;

with Classifier_Utilities;
with Encode_Utils;
--  with Printing;

package body Label is

    --     type CSR_Matrix (Num_NZ, IP_Length : Positive) is record
    --        Data    : Integer_Array (1 .. Num_NZ);
    --        Indices : Integer_Array (1 .. Num_NZ);
    --        Ind_Ptr : Integer_Array (1 .. IP_Length);
    --     end record;

    --  -------------------------------------------------------------------------

    --     function Cum_Sum (A : Boolean_Array) return Integer_Array is
    --        Result : Integer_Array (A'Range);
    --        Sum    : Natural := 0;
    --     begin
    --        for index in A'Range loop
    --           if A (index) then
    --              Sum := Sum + 1;
    --           end if;
    --           Result (index) := Sum;
    --        end loop;
    --
    --        return Result;
    --
    --     end Cum_Sum;

    --  -------------------------------------------------------------------------

    procedure Fit (Binarizer : in out Label_Binarizer; Y : Integer_Array) is
    --         Routine_Name : constant String := "Label.Binarizer Fit ";
    begin
        Binarizer.Y_Kind := Multiclass_Utils.Type_Of_Target (Y);
        Binarizer.Classes := Multiclass_Utils.Unique_Labels (Y);

    end Fit;

    --  -------------------------------------------------------------------------

    procedure Fit (Binarizer : in out Label_Binarizer; Y : Integer_Matrix) is
    --         Routine_Name : constant String := "Label.Binarizer Fit ";
    begin
        --        Assert (Binarizer.Neg_Label < Binarizer.Pos_Label, Routine_Name &
        --                  "Binarizer.Neg_Label" & Integer'Image (Binarizer.Neg_Label) &
        --                  " must be less than Binarizer.Pos_Label"
        --                & Integer'Image (Binarizer.Pos_Label));
        Binarizer.Y_Kind := Multiclass_Utils.Type_Of_Target (Y);
        Binarizer.Classes := Multiclass_Utils.Unique_Labels (Y);

    end Fit;

    --  -------------------------------------------------------------------------

    procedure Fit (Encoder : in out Label_Encoder; Y : Integer_Array) is
        Routine_Name : constant String := "Label.Encoder Fit ";
    begin
        Assert (Encoder.Encoder_Kind = Class_Unique, Routine_Name &
                  "Label.Fit called with label encoder instead of unique encode");
        Encoder.Uniques := Encode_Utils.Unique (Y);
    end Fit;

    --  -------------------------------------------------------------------------

    function Fit_Transform (Binarizer : in out Label_Binarizer;
                            Y         : Integer_Array) return Boolean_Matrix is
    begin
        Fit (Binarizer, Y);
        return Transform (Binarizer, Y);

    end Fit_Transform;

    --  -------------------------------------------------------------------------
    --  Fit_Transform fits label encoder and returns encoded labels
    --  Balanced class weights should be given by
    --  n_samples / (n_classes * np.bincount(y))
    function Fit_Transform (Encoder : in out Label_Encoder; Y : Integer_Array)
                            return Natural_Array is
        Encoded_Labels : Natural_Array (1 .. Y'Length);
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
    --  L593 Multiclass uses the maximal score instead of a threshold.
    function Inverse_Binarize_Multiclass (Y       : Boolean_Matrix;
                                          Classes : NL_Types.Integer_List)
                                          return Real_Float_Matrix is
        use Classifier_Utilities;
        --        Routine_Name :  constant String :=
        --                         "Label.Inverse_Binarize_Multiclass ";
        Inverse      : Real_Float_Matrix  (Y'Range (2), Y'Range);
        Max_Indices  : Natural_Array (Y'Range (2));
    begin
        --  L627
        Max_Indices := Row_Max_Indices (Y);
        for row in Max_Indices'Range loop
            for col in Max_Indices'Range loop
                Inverse (row, col) :=
                  Float (Classes.Element (Max_Indices (row)));
            end loop;
        end loop;

        return Inverse;

    end Inverse_Binarize_Multiclass;

    --  -------------------------------------------------------------------------
    --  L593 Multiclass uses the maximal score instead of a threshold.
    function Inverse_Binarize_Multiclass (Y       : Boolean_Matrix;
                                          Classes : NL_Types.Integer_List)
                                          return Integer_Matrix is
        use Classifier_Utilities;
        --        Routine_Name :  constant String :=
        --                         "Label.Inverse_Binarize_Multiclass ";
        Inverse      : Integer_Matrix  (Y'Range (2), Y'Range);
        Max_Indices  : Natural_Array (Y'Range (2));
    begin
        --  L627
        Max_Indices := Row_Max_Indices (Y);
        for row in Max_Indices'Range loop
            for col in Max_Indices'Range loop
                Inverse (row, col) := Classes.Element (Max_Indices (row));
            end loop;
        end loop;

        return Inverse;

    end Inverse_Binarize_Multiclass;

    --  -------------------------------------------------------------------------

    --  L593 Multiclass uses the maximal score instead of a threshold.
    --     function Inverse_Binarize_Multiclass (Y       : Float_Matrix;
    --                                           Classes : NL_Types.Integer_List)
    --                                            return Float_Matrix is
    --        use Classifier_Utilities;
    --        Routine_Name :  constant String :=
    --                         "Label.Inverse_Binarize_Multiclass ";
    --        Inverse      : Float_Matrix  (Y'Range, 1 .. 1);
    --        Max_Indices  : Natural_Array (Y'Range);
    --     begin
    --        --  L627
    --        Max_Indices := Row_Max_Indices (Y);
    --        for row in Inverse'Range loop
    --            Inverse (row, 1) := Float (Classes.Element (Max_Indices (row)));
    --        end loop;
    --
    --        return Inverse;
    --
    --     end Inverse_Binarize_Multiclass;

    --  -------------------------------------------------------------------------

    --  L586 Multiclass uses the maximal score instead of a threshold.
    function Inverse_Binarize_Multiclass (Y       : Real_Float_Matrix;
                                          Classes : NL_Types.Integer_List)
                                          return Integer_Matrix is
        use Classifier_Utilities;
        --        Routine_Name :  constant String :=
        --                         "Label.Inverse_Binarize_Multiclass ";
        Inverse      : Integer_Matrix  (Y'Range, 1 .. 1);
        Max_Indices  : Natural_Array (Y'Range);
    begin
        --  L627
        Max_Indices := Row_Max_Indices (Y);
        for row in Inverse'Range loop
            Inverse (row, 1) := Classes.Element (Max_Indices (row));
        end loop;

        return Inverse;

    end Inverse_Binarize_Multiclass;

    --  -------------------------------------------------------------------------

    --     function Inverse_Binarize_Thresholding
    --       (Y       : Real_Float_Matrix; Output_Type : Multiclass_Utils.Y_Type;
    --        Classes : NL_Types.Integer_List; Threshold : Float)
    --        return Integer_Array is
    --        use Multiclass_Utils;
    --        Routine_Name :  constant String := "Label.Inverse_Binarize_Thresholding ";
    --        Inverse      : Integer_Array (Y'Range);
    --     begin
    --        Put_Line (Routine_Name);
    --        for index in Inverse'Range loop
    --           Inverse (index) := Integer (Y (index));
    --        end loop;
    --        if Output_Type = Y_Binary then
    --           if Y'Length = 2 then
    --              null;
    --           else
    --              null;
    --           end if;
    --
    --        elsif Output_Type = Y_Multilabel_Indicator then
    --           null;
    --        else
    --           Assert (False, Routine_Name & Y_Type'Image (Output_Type) &
    --                  " format is not supported");
    --        end if;
    --
    --        return Inverse;
    --
    --     end Inverse_Binarize_Thresholding;

    --  -------------------------------------------------------------------------

    function Inverse_Transform (Self : Label_Binarizer; Y : Boolean_Matrix)
                                return Real_Float_Matrix is
        use Multiclass_Utils;
        Y_Inv     : Real_Float_Matrix (1 .. Y'Length (2), 1 .. Y'Length);
        --        Threshold : Float := (Self.Pos_Label + Self.Neg_Label) / 2.0;
    begin
        if Self.Y_Kind = Y_Multiclass then
            Y_Inv := Inverse_Binarize_Multiclass (Y, Self.Classes);
        else
            null;
            --   Y_Inv := Inverse_Binarize_Thresholding (Y, Self.Classes, Threshold);
        end if;

        return Y_Inv;

    end Inverse_Transform;

    --  -------------------------------------------------------------------------

    --     function Inverse_Transform (Self : Label_Binarizer; Y : Float_Matrix)
    --                                  return Float_Matrix is
    --        use Multiclass_Utils;
    --        Y_Inv     : Float_Matrix (1 .. Y'Length, 1 .. 1);
    --        Threshold : Float := (Self.Pos_Label + Self.Neg_Label) / 2.0;
    --     begin
    --        if Self.Y_Kind = Y_Multiclass then
    --           Y_Inv := Inverse_Binarize_Multiclass (Y, Self.Classes);
    --        else
    --           null;
    --           --   Y_Inv := Inverse_Binarize_Thresholding (Y, Self.Classes, Threshold);
    --        end if;
    --
    --        return Y_Inv;
    --
    --     end Inverse_Transform;

    --  -------------------------------------------------------------------------

    function Inverse_Transform (Self : Label_Binarizer; Y : Boolean_Matrix)
                                return Integer_Matrix is
        use Multiclass_Utils;
        Y_Inv     : Integer_Matrix (1 .. Y'Length (2), 1 .. Y'Length);
        --        Threshold : Float := (Self.Pos_Label + Self.Neg_Label) / 2.0;
    begin
        if Self.Y_Kind = Y_Multiclass then
            Y_Inv := Inverse_Binarize_Multiclass (Y, Self.Classes);
        else
            null;
            --   Y_Inv := Inverse_Binarize_Thresholding (Y, Self.Classes, Threshold);
        end if;

        return Y_Inv;

    end Inverse_Transform;

    --  -------------------------------------------------------------------------

    --   Inverse_Transform transforms labels back to original encoding
    function Inverse_Transform (Self : Label_Encoder; Labels : Natural_Array)
                                return Integer_Array is
        use NL_Types;
        Routine_Name :  constant String := "Label.Inverse_Transform ";
        aRange       : Integer_Array (1 .. Positive (Self.Uniques'Length));
        Diff         : Natural_List;
        Result       : Integer_Array (1 .. Positive (Labels'Length));
    begin
        Put_Line (Routine_Name);
        for index in aRange'Range loop
            aRange (index) := index;
        end loop;

        Diff := Classifier_Utilities.Set_Diff (Labels, aRange);
        Assert (Diff.Is_Empty,
                "Label.Inverse_Transform Labels vector contains " &
                  "previously unseen labels.");

        for index in Result'Range loop
            Result (index) := Self.Uniques (Labels (index));
        end loop;

        return Result;

    end Inverse_Transform;

    --  -------------------------------------------------------------------------

    function Inverse_Transform (Self : Label_Encoder; Y : Integer_Array)
                                return Integer_Array is
        aRange  : Integer_Array (1 .. Positive (Self.Uniques'Length));
        Diff    : NL_Types.Natural_List;
        Result  : Integer_Array (1 .. Positive (Y'Length));
    begin
        for index in Self.Classes'Range loop
            aRange (index) := index;
        end loop;

        Diff := Classifier_Utilities.Set_Diff (Y, aRange);
        Assert (Diff.Is_Empty, "Y contains previously unseen labels.");

        for index in Y'Range loop
            Result (index) := Self.Classes (Y (index));
        end loop;

        return Result;

    end Inverse_Transform;

    --  -------------------------------------------------------------------------

    function Inverse_Transform (Self : Label_Encoder; Y : Integer_Matrix)
                                return Integer_Matrix is
        use NL_Types;
        YT        : constant Integer_Matrix := Transpose (Y);
        aRange    : Integer_Array (1 .. Y'Length);
        Diff      : Natural_List;
        Transform : Integer_Matrix (1 .. YT'Length, 1 .. YT'Length (2));
        YT_Row    : Integer_Array (1 .. YT'Length);
    begin
        for r in Self.Classes'Range loop
            aRange (r) := r;
        end loop;

        for Y_index in YT'Range loop
            for index2 in YT'Range (2) loop
                YT_Row (Y_index) := YT (Y_index, index2);
            end loop;
            Diff := Classifier_Utilities.Set_Diff (YT_Row, aRange);
            Assert (Diff.Is_Empty, "Y contains previously unseen labels.");

            for index2 in YT_Row'Range loop
                Transform (Y_index, index2) := Self.Classes (YT_Row (index2));
            end loop;
        end loop;

        return Transpose (Transform);

    end Inverse_Transform;

    --  -------------------------------------------------------------------------
    --  L361
    function Inverse_Transform (Self : Label_Binarizer; Y : Real_Float_Matrix)
                                return Integer_Matrix is
        use Multiclass_Utils;
        --        Threshold : constant Float := (Self.Pos_Label + Self.Neg_Label) / 2.0;
        Y_Inv     : Integer_Matrix (1 .. Y'Length, 1 .. 1);
    begin
        --  L398
        if Self.Y_Kind = Y_Multiclass then
            Y_Inv := Inverse_Binarize_Multiclass (Y, Self.Classes);
        else
            null;
            --           Y_Inv := Inverse_Binarize_Thresholding (Y, Self.Classes, Threshold);
        end if;

        return Y_Inv;

    end Inverse_Transform;

    --  -------------------------------------------------------------------------
    --  L361 Inverse_Transform transforms binary labels back to
    --       multi-class labels
    --     function Inverse_Transform (Self : Label_Binarizer; Y : Boolean_Matrix;
    --                                 Use_Threshold : Boolean := False;
    --                                 Threshold     : Float := 0.0)
    --                                  return Boolean_Matrix is
    --        Routine_Name :  constant String := "Label.Inverse_Transform Binarizer ";
    --        use Multiclass_Utils;
    --        Y_Inv  : Boolean_Matrix (Y'First (2) .. Y'Last (2), Y'First .. Y'Last);
    --        Thresh : Float;
    --     begin
    --        Put_Line (Routine_Name);
    --        if Self.Y_Kind = Y_Multiclass then
    --  --           Y_Inv := Inverse_Binarize_Multiclass (Y, Self.Classes);
    --           null;
    --        else
    --           if Use_Threshold then
    --              Thresh := Threshold;
    --           else
    --              Thresh := 0.5 * (Self.Neg_Label + Self.Pos_Label);
    --           end if;
    --
    --           Y_Inv := Inverse_Binarize_Thresholding (Y, Self.Classes, Thresh);
    --        end if;
    --
    --        return Y_Inv;
    --
    --     end Inverse_Transform;

    --  -------------------------------------------------------------------------
    --  L416
    function Label_Binarize (Y, Classes : NL_Types.Integer_List;
                             Neg_Label : Integer := 0) return Boolean_Matrix is
        use Ada.Containers;
        use Multiclass_Utils;
        Routine_Name :  constant String := "Label.Label_Binarize ";
        Num_Classes  : constant Positive := Positive (Classes.Length);
        Y_Bin        : Boolean_Matrix (Y.First_Index .. Y.Last_Index,
                                       1 .. Positive (Classes.Length)) :=
                         (others => (others => False));
        Y_Kind       : Multiclass_Utils.Y_Type := Type_Of_Target (Y);
        Sorted       : NL_Types.Integer_List;
        Done         : Boolean := False;

        function Binarize (Y_In : NL_Types.Integer_List)
                           return Boolean_Matrix is
            Result : Boolean_Matrix
              (1 .. Positive (Y_In.Length), 1 .. Positive (Classes.Length))
              := (others => (others => False));
        begin
            for row in Y_In.First_Index .. Y_In.Last_Index loop
                for col in Classes.First_Index .. Classes.Last_Index loop
                    if Y_In (row) = Classes (col) then
                        Result (row, col) := True;
                    end if;
                end loop;
            end loop;

            return Result;

        end Binarize;

    begin
        Assert (Y_Kind /= Y_Unknown, Routine_Name & "unknown target data type.");
        --  L506
        Assert (Y_Kind /= Y_Continuous_Multioutput and
                  Y_Kind /= Y_Multiclass_Multioutput, Routine_Name &
                  "does not support Multioutput target data.");

        --  L516
        if Y_Kind = Y_Binary then
            if Num_Classes = 1 then
                for row in Y.First_Index .. Y.Last_Index loop
                    for col in Classes.First_Index .. Classes.Last_Index loop
                        if Neg_Label /= 0 then
                            Y_Bin (row, col) := True;
                        end if;
                    end loop;
                end loop;
                Done := True;
            elsif Num_Classes > 2 then
                Y_Kind := Y_Multiclass;
            end if;
        end if;

        --  L529
        if Y_Kind = Y_Multilabel_Indicator then
            Assert (Classes.Length = Y_Bin'Length (2),
                    Routine_Name & "L529 class size" &
                      Count_Type'Image (Classes.Length) & " is different to Y size"
                    & Integer'Image (Y_Bin'Length (2)));
        end if;

        if not Done then
            --  L528
            Sorted := Classes;
            NL_Types.Integer_Sorting.Sort (Sorted);
            --  L538
            if Y_Kind = Y_Binary or Y_Kind = Y_Multiclass then
                --  Label.py L539 - L549 needed to generate a csr sparse matrix
                --  Binarize is all that is needed for this implementation
                Y_Bin := Binarize (Y);

            else
                Assert (Y_Kind = Y_Multilabel_Indicator, Routine_Name &
                          Y_Type'Image (Y_Kind) &
                          " target data is not supported by Label_Binarize");
                Y_Bin := Binarize (Y);
            end if;
        end if;

        --          Printing.Print_Boolean_Matrix (Routine_Name & " result Y_Bin", Y_Bin);
        return Y_Bin;

    end Label_Binarize;

    --  -------------------------------------------------------------------------
    --  L416
    function Label_Binarize (Y         : Integer_Array;
                             Classes   : NL_Types.Integer_List;
                             Neg_Label : Integer := 0) return Boolean_Matrix is
        use Ada.Containers;
        use Multiclass_Utils;
        Routine_Name :  constant String := "Label.Label_Binarize ";
        --        Num_Samples  : constant Positive := Y'Length;
        Num_Classes  : constant Positive := Positive (Classes.Length);
        Y_Bin        : Boolean_Matrix (Y'Range, 1 .. Positive (Classes.Length)) :=
                         (others => (others => False));
        Y_Kind       : Multiclass_Utils.Y_Type := Type_Of_Target (Y);
        Sorted       : NL_Types.Integer_List;
        Done         : Boolean := False;

        function Binarize (Y_In : Integer_Array) return Boolean_Matrix is
            Result                     : Boolean_Matrix (Y_In'Range, 1 ..
                                                           Positive (Classes.Length)) :=
                                           (others => (others => False));
        begin
            for row in Y_In'Range loop
                for col in Classes.First_Index .. Classes.Last_Index loop
                    if Y_In (row) = Classes (col) then
                        Result (row, col) := True;
                    end if;
                end loop;
            end loop;

            return Result;

        end Binarize;

    begin
        Assert (Y_Kind /= Y_Unknown, Routine_Name & "unknown target data type.");
        --  L506
        Assert (Y_Kind /= Y_Continuous_Multioutput and
                  Y_Kind /= Y_Multiclass_Multioutput, Routine_Name &
                  "does not support Multioutput target data.");

        --  L516
        if Y_Kind = Y_Binary then
            if Num_Classes = 1 then
                for row in Y'Range loop
                    for col in Classes.First_Index .. Classes.Last_Index loop
                        if Neg_Label /= 0 then
                            Y_Bin (row, col) := True;
                        end if;
                    end loop;
                end loop;
                Done := True;
            elsif Num_Classes > 2 then
                Y_Kind := Y_Multiclass;
            end if;
        end if;

        --  L529
        if Y_Kind = Y_Multilabel_Indicator then
            Assert (Classes.Length = Y_Bin'Length (2),
                    Routine_Name & "L529 class size" &
                      Count_Type'Image (Classes.Length) & " is different to Y size"
                    & Integer'Image (Y_Bin'Length (2)));
        end if;

        if not Done then
            --  L528
            Sorted := Classes;
            NL_Types.Integer_Sorting.Sort (Sorted);
            --  L538
            if Y_Kind = Y_Binary or Y_Kind = Y_Multiclass then
                --  Label.py L539 - L549 needed to generate a csr sparse matrix
                --  Binarize is all that is needed for this implementation
                Y_Bin := Binarize (Y);

            else
                Assert (Y_Kind = Y_Multilabel_Indicator, Routine_Name &
                          Y_Type'Image (Y_Kind) &
                          " target data is not supported by Label_Binarize");
                Y_Bin := Binarize (Y);
            end if;
        end if;

        --          Printing.Print_Boolean_Matrix (Routine_Name & " result Y_Bin", Y_Bin);
        return Y_Bin;

    end Label_Binarize;

    --  -------------------------------------------------------------------------

    function Transform (Self : Label_Binarizer; Y : Integer_Array)
                        return Boolean_Matrix is
    --  Routine_Name : constant String := "Label.Transform Binarize ";
    begin
        --  Printing.Print_Integer_List (Routine_Name & "Classes", Self.Classes);

        return Label_Binarize (Y, Self.Classes);

    end Transform;

    --  -------------------------------------------------------------------------

    function Transform (Self : Label_Binarizer; Y : NL_Types.Integer_List)
                        return Boolean_Matrix is
    --  Routine_Name : constant String := "Label.Transform Binarize ";
    begin

        return Label_Binarize (Y, Self.Classes);

    end Transform;

    --  -------------------------------------------------------------------------

    function Transform (Self : Label_Binarizer; Y : NL_Types.Integer_List_2D)
                        return Boolean_Matrix is
      use NL_Types;
      use Integer_Package;
      --  Routine_Name : constant String := "Label.Transform Binarize ";
      Y_Row         : Integer_List;
      Classes       : Integer_List;
      Indices       : Integer_List_2D;
      Class_Curs    : Cursor;
      Class         : Integer;
      Max_Index     : Positive := 1;
      Classes_Array : array (1 .. Positive (Y.Length)) of Integer_List;
    begin
      for row in Y.First_Index .. Y.Last_Index loop
         Y_Row := Y.Element (row);
         Classes.Clear;
         for col in Y_Row.First_Index .. Y_Row.Last_Index loop
            Class_Curs := Self.Classes.Find (Y_Row (col));
            Class := Element (Class_Curs);
            Classes.Append (Class);
         end loop;

         if Positive (Classes.Length) > Max_Index then
            Max_Index := Positive (Classes.Length);
         end if;

         Indices.Append (Classes);
         Classes_Array (row) := Classes;
      end loop;

      declare
         Bool_List : Integer_List;
         Result    : Boolean_Matrix (1 .. Positive (Y.Length), 1 .. Max_Index);
      begin
         for row in Result'Range loop
            Bool_List := Classes_Array (row);
            for col in Bool_List.First_Index .. Bool_List.Last_Index loop
               Result (row, col) := Bool_List.Element (col);
            end loop;
         end loop;
         return Result;
      end;

    end Transform;

    --  -------------------------------------------------------------------------
    --  Transform returns labels as normalized encodings
    function Transform (Self : Label_Encoder; Y : Integer_Array)
                        return Natural_Array is
        Labels : Natural_Array (1 .. Y'Length);
    begin
        if Self.Encoder_Kind = Class_Unique then
            Labels := Encode_Utils.Encode (Y, Self.Uniques);
        else
            raise Label_Error with
              "Label.Transform called with invalid encoder type.";
        end if;

        return Labels;

    end Transform;

    --  -------------------------------------------------------------------------

end Label;
