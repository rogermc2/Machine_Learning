
with Classifier_Utilities;
with Label;

package body Weights is

    function Get_Column (Weights  : Weight_Lists_List; Data_Index : Positive;
                         Data     : out Weight_Data) return  Float_Array;
    function Reduce_Weight_Lists (Lists : Weight_Lists_List)
                                  return Weight_List;

    --  -------------------------------------------------------------------------
    --  Compute_Class_Weight estimates class weights for unbalanced datasets.
    function Compute_Class_Weights (Class_Weight : Weight_Type;
                                    Class_Weights : Weight_List;
                                    Y            : ML_Types.Value_Data_List;
                                    Classes      : ML_Types.Value_Data_List)
                                    return Weight_List is
        Y_Length   : constant Integer := Integer (Y.Length);
        Weights    : Weight_List;
        LE         : Label.Label_Encoder;
        Y_Ind      : Natural_List;
        Weight     : Weight_Data := (To_Unbounded_String (""), 1.0);
        Recip_Freq : Natural_List;
        Recip      : Natural;
    begin
        if Class_Weight = No_Weight or Class_Weights.Is_Empty then
            for index in Classes.First_Index .. Classes.Last_Index loop
                Weights.Append (Weight);
            end loop;

        elsif Class_Weight = Balanced_Weight then
            --  Find the weight of each class  present in Y.
            Y_Ind := Label.Fit_Transform (LE, Y);
            Recip_Freq := Classifier_Utilities.Bin_Count (Y_Ind);
            for index in Recip_Freq.First_Index .. Recip_Freq.Last_Index loop
                Recip := Natural (Float (Y.Length) / Float (LE.Classes.Length)) *
                  Recip_Freq.Element (index);
                Recip_Freq.Replace_Element (index, Recip);
            end loop;

        else  --  user-defined dictionary
            null;
        end if;

        return Weights;

    end Compute_Class_Weights;

    --  -------------------------------------------------------------------------
    --  Compute_Sample_Weight estimates sample weights by class for
    --  unbalanced datasets.
    --  y : array-like of shape (n_samples,) or (n_samples, n_outputs)
    --   Array of original class labels per sample.
    --  Class_Weight : dict, list of dicts, "balanced", or None, optional
    --     Weights associated with classes
    --  Indices : list of indices to be used in a subsample
    function Compute_Sample_Weight (Weight_Kind    : Weight_Type;
                                    Y              : ML_Types.List_Of_Value_Data_Lists;
                                    Class_Weights  : Weight_List :=
                                      Weight_Package.Empty_Vector;
                                    Indices        : Integer_List :=
                                      Integer_Package.Empty_Vector)
                                return Weight_List is
        use ML_Types;
        use Value_Data_Package;
        use Value_Data_Sorting;
        --        Y_Lists_Curs          : Value_Lists_Data_Package.Cursor;
        Num_Outputs           : constant Integer := Integer (Y.Length);
        Y_Full                : Value_Data_List;
        Classes_Full          : Value_Data_List;
        aClass                : Value_Record;
        Classes_Missing       : Value_Data_List;
        Class_Weight_K        : Float;
        Y_Subsample           : Value_Data_List;
        Classes_Subsample     : Value_Data_List;
        Weight_K              : Weight_List;
        aWeight               : Weight_Data;
        K_Indices             : Integer_List;
        Class_K_Weights       : Weight_List;
        Expanded_Class_Weight : Weight_Lists_List;
    begin
        if Weight_Kind /= Balanced_Weight then
            raise Weights_Error with
              "Compute_Sample_Weight; Weight does not contain the only" &
              " valid preset for class_weight which is balanced.";
        end if;

        if Num_Outputs > 1 and then
          Integer (Class_Weights.Length) /= Num_Outputs then
            raise Weights_Error with
              "Weights.Compute_Sample_Weight; For multi-output, number of " &
              "elements in class_weight should match number of outputs.";
        end if;

        for index_k in 1 .. Num_Outputs loop
            --  y_full = y[:, k]
            Y_Full := Classifier_Utilities.Get_Column (Y, index_k);
            Classes_Full := Classifier_Utilities.Unique_Values (Y_Full);
            Classes_Missing.Clear;

            if Weight_Kind = Balanced_Weight or Num_Outputs = 1 then
                Class_Weight_K := Class_Weights.First_Element.Weight;
            else
                Class_Weight_K := Class_Weights.Element (index_k).Weight;
            end if;

            if Indices.Is_Empty then
                Weight_K := Compute_Class_Weights
                  (Weight_Kind, Y_Subsample, Classes_Subsample);
            else
                for index in Indices.First_Index .. Indices.Last_Index loop
                    Y_Subsample.Append (Y.Element (Indices.Element (index)));
                end loop;

                Classes_Subsample := Classifier_Utilities.Unique_Values (Y_Subsample);
                --  Get class weights for the subsample covering all classes in
                --  case some labels present in the original data are missing
                --  from the sample.
                Class_K_Weights := Compute_Class_Weights
                  (Weight_Kind, Y_Subsample, Classes_Subsample);

                --  weight_k = weight_k[np.searchsorted(classes_full, y_full)]
                K_Indices := Classifier_Utilities.Search_Sorted_Value_List
                  (Classes_Full, Y_Full);
                Class_K_Weights := Weight_K;
                Weight_K.Clear;
                for index in Class_K_Weights.First_Index ..
                  Class_K_Weights.Last_Index loop
                    Weight_K.Append (Class_K_Weights (index));
                end loop;
                --  TO DO

                Weight_K := Class_K_Weights;
                for index in Classes_Full.First_Index ..
                  Classes_Full.Last_Index loop
                    aClass := Classes_Full (index);
                    if Find (Classes_Subsample, aClass) =
                      Value_Data_Package.No_Element then
                        Classes_Missing.Append (aClass);
                    end if;
                end loop;

            end if;

            if not Classes_Missing.Is_Empty then
                --  Make missing classes weights zero
                for index in Y_Full.First_Index .. Y_Full.Last_Index loop
                    aClass := Y_Full (index);
                    if not (Find (Classes_Missing, aClass) =
                              Value_Data_Package.No_Element) then
                        aWeight := Weight_K (index);
                        aWeight.Weight := 0.0;
                        Weight_K.Replace_Element (index, aWeight);
                    end if;
                end loop;
            end if;
            Expanded_Class_Weight.Append (Weight_K);
        end loop;

        return Reduce_Weight_Lists (Expanded_Class_Weight);

    end Compute_Sample_Weight;

    --  -------------------------------------------------------------------------

    function Get_Column (Weights  : Weight_Lists_List; Data_Index : Positive;
                         Data     : out Weight_Data)
                     return  Float_Array is
        aList  : Weight_List;
        Column : Float_Array (1 .. integer (Weights.Length));
    begin
        for index in 1 .. integer (Weights.Length) loop
            aList := Weights.Element (index);
            Data := aList.Element (Data_Index);
            Column (index) := Data.Weight;
        end loop;
        return Column;
    end Get_Column;

    --  -------------------------------------------------------------------------

    function Reduce_Weight_Lists (Lists : Weight_Lists_List)
                              return Weight_List is
        Col     : Float_Array (Lists.First_Index .. Lists.Last_Index);
        Product : Float;
        theList : Weight_List;
        Data    : Weight_Data;
    begin
        for index in Lists.First_Index .. Lists.Last_Index loop
            Col := Get_Column (Lists, index, Data);
            Product := 1.0;
            for col_index in Col'Range loop
                Product := Product * Col (col_index);
            end loop ;
            Data.Weight := Product;
            theList.Append (Data);
        end loop;
        return theList;
    end Reduce_Weight_Lists;

    --  -------------------------------------------------------------------------

end Weights;
