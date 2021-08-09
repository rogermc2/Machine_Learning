
with Classifier_Utilities;
with Encoder;

package body Weights is

   function Get_Column (Data  : Weight_Lists_List; Data_Index : Positive)
                        return  Float_Array;
   function Reduce_Weight_Lists (Lists : Weight_Lists_List)
                                 return Weight_List;

   --  -------------------------------------------------------------------------
   --  Compute_Class_Weight estimates class weights for unbalanced datasets.
   function Compute_Class_Weights (Class_Weight : Weight_Type;
                                   Y            : ML_Types.Value_Data_List;
                                   Classes      : ML_Types.Value_Data_List)
                                   return Weight_List is
      Weights : Weight_List;
      Weight  : Weight_Data :=  (To_Unbounded_String (""), 1.0);
      LE      : Encoder.Label_Encoder;
      Y_Ind   : Integer_List;
   begin
      if Class_Weight = No_Weight then
         for index in Classes.First_Index .. Classes.Last_Index loop
            Weights.Append (Weight);
         end loop;
      elsif Class_Weight = Balanced_Weight then
         --           Y_Ind := Encoder.Fit_Transform (LE, Y);
         null;
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
                                   return Weight_Lists_List is
      use ML_Types;
      use Value_Data_Package;
      use Value_Data_Sorting;
      Y_Lists_Curs          : Value_Lists_Data_Package.Cursor;
      Sample_Weights        : Weight_Map;
      Num_Outputs           : Integer := Integer (Y.Length);
      Y_Array               : Value_Data_List;
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
      Sample_Weight         : Float_List;
      Expanded_Class_Weight : Weight_Lists_List;
   begin
      --        if Class_Weight /= Balanced_Weight then
      --           raise Value_Error with
      --             "Compute_Sample_Weight; Weight does not contain the only" &
      --             " valid preset for class_weight which is balanced.";
      --        end if;

      if Num_Outputs > 1 and then
        Integer (Class_Weights.Length) /= Num_Outputs then
         raise Weights_Error with
           "Compute_Sample_Weight; For multi-output, number of elements in " &
           "class_weight should match number of outputs.";
      end if;

      for index_k in 1 .. Num_Outputs loop
         --  y_full = y[:, k]
         Y_Full := Classifier_Utilities.Get_Column (Y, index_k);
         Classes_Full := Classifier_Utilities.Unique_Values (Y_Full);
         Classes_Missing.Clear;
         if Weight_Kind = Balanced_Weight or Num_Outputs = 1 then
            Class_Weight_K := 1.0;
            --                 Class_Weight_K := Class_Weight;
         else
            Class_Weight_K := Class_Weights.Element (index_k).Weight;
         end if;

         if not Is_Sorted (Classes_Full) then
            Sort (Classes_Full);
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
         end if;
         Expanded_Class_Weight.Append (Weight_K);
      end loop;

      return Expanded_Class_Weight;
   end Compute_Sample_Weight;

   --  -------------------------------------------------------------------------

   function Get_Column (Data  : Weight_Lists_List; Data_Index : Positive)
                        return  Float_Array is
      aList  : Weight_List;
      Column : Float_Array (1 .. integer (Data.Length));
   begin
      for index in 1 .. integer (Data.Length) loop
         aList := Data.Element (index);
         Column (index) := aList.Element (Data_Index).Weight;
      end loop;
      return Column;
   end Get_Column;

   --  -------------------------------------------------------------------------

   function Reduce_Weight_Lists (Lists : Weight_Lists_List)
                                 return Weight_List is
      Col     : array (Lists.First_Index .. Lists.Last_Index) of Float;
      Product : Float;
      aRow    : Weight_List;
      theList : Weight_List;
   begin
      for row in Lists.First_Index .. Lists.Last_Index loop
         Product := 1.0;
         aRow := Lists.Element (row);
         col (Lists.First_Index) := Product * aRow.Element (Lists.First_Index).Weight;
      end loop;
      return theList;
   end Reduce_Weight_Lists;

   --  -------------------------------------------------------------------------

end Weights;
