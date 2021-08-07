
with Ada.Containers.Ordered_Sets;
with Ada.Text_IO; use Ada.Text_IO;

with Encoder;

package body Classifier_Utilities is

   package Bool_Sets is new Ada.Containers.Ordered_Sets (Boolean);
   package Float_Sets is new Ada.Containers.Ordered_Sets (Float);
   package Int_Sets is new Ada.Containers.Ordered_Sets (Integer);
   package UB_String_Sets is new Ada.Containers.Ordered_Sets (Unbounded_String);

   --  -------------------------------------------------------------------------

   procedure Clear (anArray : in out ML_Types.Label_Data_Array) is
   begin
      for index in anArray'Range loop
         anArray (index).Float_Value := 0.0;
      end loop;
   end Clear;

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
                                   return Float_List is
      use ML_Types;
      use Integer_Package;
      use Value_Data_Package;
      use Value_Lists_Data_Package;
      Y_Lists_Curs          : Value_Lists_Data_Package.Cursor := Y.First;
      Expanded_Class_Weight : Weight_Map;
      Sample_Weights        : Weight_Map;
      Num_Outputs           : Integer := Integer (Y.Length);
      Y_Array               : Value_Data_List;
      Y_Full                : Value_Data_List;
      Classes_Full          : Value_Data_List;
      Class_Weight_K        : Float;
      --        Weight_K              : Float;
      Y_Subsample           : Value_Data_List;
      Classes_Subsample     : Value_Data_List;
      Weight_K              : Float;
      Class_K_Weights       : Weight_List;
      Sample_Weight         : Float_List;
   begin
      --        if Class_Weight /= Balanced_Weight then
      --           raise Value_Error with
      --             "Compute_Sample_Weight; Weight does not contain the only" &
      --             " valid preset for class_weight which is balanced.";
      --        end if;

      if Num_Outputs > 1 and then
        Integer (Class_Weights.Length) /= Num_Outputs then
         raise Value_Error with
           "Compute_Sample_Weight; For multi-output, number of elements in " &
           "class_weight should match number of outputs.";
      end if;

      for index_k in 1 .. Num_Outputs loop
         --  y_full = y[:, k]
         while Has_Element (Y_Lists_Curs) loop
            Y_Full.Append (Element (Y_Lists_Curs));
            Next (Y_Lists_Curs);
         end loop;
         Classes_Full := Unique_Values (Y_Full);
         if Weight_Kind = Balanced_Weight or Num_Outputs = 1 then
            Class_Weight_K := 1.0;
            --                 Class_Weight_K := Class_Weight;
         else
            Class_Weight_K := Class_Weights.Element (index_k).Weight;
         end if;

         if Indices.Is_Empty then
            null;
         else
            --  Get class weights for the subsample, covering all classes in
            --  case some labels that were present in the original data are
            --  missing from the sample.
            Y_Subsample.Append (Y.Element (Indices.Element (index_k)));
            Classes_Subsample.Append (Unique_Values (Y_Subsample));
            Class_K_Weights := Compute_Class_Weights
              (Weight_Kind, Y_Subsample, Classes_Subsample);
            Weight_K := 0.0;
         end if;
      end loop;

      --        for index in Y.First_Index .. Num_Outputs loop
      --           Y_Full := Y_Array (index);
      --        end loop;

      return Sample_Weight;
   end Compute_Sample_Weight;

   --  -------------------------------------------------------------------------

   function To_Array (L : Integer_List) return Integer_Array is
      New_Array : Integer_Array (1 .. Integer (L.Length));
      A_Index   : Integer := 0;
   begin
      for index in L.First_Index .. L.Last_Index loop
         A_Index := A_Index + 1;
         New_Array (A_Index) := L.Element (index);
      end loop;
      return New_Array;
   end To_Array;

   --  -------------------------------------------------------------------------

   function To_Float_List (A : Float_Array) return Float_List is
      A_List : Float_List;
   begin
      for index in A'Range loop
         A_List.Append (A (index));
      end loop;
      return A_List;
   end To_Float_List;

   --  -------------------------------------------------------------------------

   function Unique (Nums : Integer_List) return Integer_List is
      use Int_Sets;
      use Integer_Package;
      Unique_Set : Int_Sets.Set;
      Int_Curs   : Integer_Package.Cursor := Nums.First;
      Set_Curs   : Int_Sets.Cursor;
      Nums_List  : Integer_List;
   begin
      while Has_Element (Int_Curs) loop
         Unique_Set.Include (Element (Int_Curs));
         Next (Int_Curs);
      end loop;

      Set_Curs := Unique_Set.First;
      while Has_Element (Set_Curs) loop
         Nums_List.Append (Element (Set_Curs));
         Next (Set_Curs);
      end loop;
      return Nums_List;
   end Unique;

   --  -------------------------------------------------------------------------

   function Unique_Values (Values : ML_Types.Value_Data_List)
                           return ML_Types.Value_Data_List is
      use ML_Types;
      use Int_Sets;
      use Value_Data_Package;
      Nums_Curs         : Value_Data_Package.Cursor := Values.First;
      Num_Value         : Value_Record;
      Bool_Value        : Value_Record (Boolean_Type);
      Float_Value       : Value_Record (Float_Type);
      Int_Value         : Value_Record (Integer_Type);
      UB_String_Value   : Value_Record (UB_String_Type);
      Unique_Booleans   : Bool_Sets.Set;
      Unique_Floats     : Float_Sets.Set;
      Unique_Integers   : Int_Sets.Set;
      Unique_UB_Strings : UB_String_Sets.Set;
      Booleans_Curs     : Bool_Sets.Cursor;
      Floats_Curs       : Float_Sets.Cursor;
      Ints_Curs         : Int_Sets.Cursor;
      UB_Strings_Curs   : UB_String_Sets.Cursor;
      Nums_List         : Value_Data_List;
   begin
      while Has_Element (Nums_Curs) loop
         Num_Value := Element (Nums_Curs);
         case Num_Value.Value_Kind is
            when Boolean_Type =>
               Unique_Booleans.Include (Num_Value.Boolean_Value);
            when Float_Type =>
               Unique_Floats.Include (Num_Value.Float_Value);
            when Integer_Type =>
               Unique_Integers.Include (Num_Value.Integer_Value);
            when UB_String_Type =>
               Unique_UB_Strings.Include (Num_Value.UB_String_Value);
         end case;
         Next (Nums_Curs);
      end loop;

      Booleans_Curs := Unique_Booleans.First;
      while Bool_Sets.Has_Element (Booleans_Curs) loop
         Bool_Value.Boolean_Value := Bool_Sets.Element (Booleans_Curs);
         Nums_List.Append (Bool_Value);
         Bool_Sets.Next (Booleans_Curs);
      end loop;
      Floats_Curs := Unique_Floats.First;
      while Float_Sets.Has_Element (Floats_Curs) loop
         Float_Value.Float_Value := Float_Sets.Element (Floats_Curs);
         Nums_List.Append (Float_Value);
         Float_Sets.Next (Floats_Curs);
      end loop;
      Ints_Curs := Unique_Integers.First;
      while Int_Sets.Has_Element (Ints_Curs) loop
         Int_Value.Integer_Value := Int_Sets.Element (Ints_Curs);
         Nums_List.Append (Int_Value);
         Int_Sets.Next (Ints_Curs);
      end loop;
      UB_Strings_Curs := Unique_UB_Strings.First;
      while UB_String_Sets.Has_Element (UB_Strings_Curs) loop
         UB_String_Value.UB_String_Value :=
           UB_String_Sets.Element (UB_Strings_Curs);
         Nums_List.Append (Bool_Value);
         UB_String_Sets.Next (UB_Strings_Curs);
      end loop;
      return Nums_List;
   end Unique_Values;

   -------------------------------------------------------------------------

   function Unique_Integer_Array (Nums : ML_Types.Label_Data_Array)
                                  return Integer_Array is
      use Int_Sets;
      use Integer_Package;
      Nums_List  : Integer_List;
      Unique_Set : Int_Sets.Set;
      Int_Curs   : Integer_Package.Cursor;
      Set_Curs   : Int_Sets.Cursor;
   begin
      for index in Nums'Range loop
         Unique_Set.Include (Nums (index).Integer_Value);
      end loop;

      declare
         Unique_Array : Integer_Array (1 .. Integer (Unique_Set.Length));
         Unique_Index : Integer := 0;
      begin
         Set_Curs := Unique_Set.First;
         while Has_Element (Set_Curs) loop
            Unique_Index := Unique_Index + 1;
            Unique_Array (Unique_Index) := Element (Set_Curs);
            Next (Set_Curs);
         end loop;
         return Unique_Array;
      end;
   end Unique_Integer_Array;

   --  -------------------------------------------------------------------------

   function Unique_Integer_Array (Nums : Integer_Array) return Integer_Array is
      use Int_Sets;
      use Integer_Package;
      Nums_List  : Integer_List;
      Unique_Set : Int_Sets.Set;
      Int_Curs   : Integer_Package.Cursor;
      Set_Curs   : Int_Sets.Cursor;
   begin
      for index in Nums'Range loop
         Unique_Set.Include (Nums (index));
      end loop;

      declare
         Unique_Array : Integer_Array (1 .. Integer (Unique_Set.Length));
         Unique_Index : Integer := 0;
      begin
         Set_Curs := Unique_Set.First;
         while Has_Element (Set_Curs) loop
            Unique_Index := Unique_Index + 1;
            Unique_Array (Unique_Index) := Element (Set_Curs);
            Next (Set_Curs);
         end loop;
         return Unique_Array;
      end;
   end Unique_Integer_Array;

   --  -------------------------------------------------------------------------

   generic
      type Index_Type is (<>);
      type Vector_Type is  array (Index_Type) of aliased Float;
   procedure Print_Floats_Vector (Name : String; aVector : Vector_Type);

   procedure Print_Floats_Vector (Name : String; aVector : Vector_Type) is
   begin
      if Name = "" then
         Put ("  ");
      else
         Put (Name & ":  ");
      end if;
      for Index in aVector'Range loop
         Put (Float'Image (aVector (Index)) & "   ");
      end loop;
      New_Line;
   end Print_Floats_Vector;

   --  -------------------------------------------------------------------

   generic
      type Index_Type is (<>);
      type Vector_Type is  array (Index_Type) of aliased Integer;
   procedure Print_Integer_Vector (Name : String; aVector : Vector_Type);

   procedure Print_Integer_Vector (Name : String; aVector : Vector_Type) is
   begin
      if Name = "" then
         Put ("  ");
      else
         Put (Name & ":  ");
      end if;
      for Index in aVector'Range loop
         Put (Integer'Image (aVector (Index)) & "   ");
      end loop;
      New_Line;
   end Print_Integer_Vector;

   --  -------------------------------------------------------------------

   procedure Print_Integer_Array (Name : String; anArray : Integer_Array) is
   begin
      Put_Line (Name & ": ");
      for Index in anArray'First .. anArray'Last loop
         Put_Line (Integer'Image (Index) & ":  " &
                     Integer'Image (anArray (Index)));
      end loop;
      New_Line;
   end Print_Integer_Array;

   --  ------------------------------------------------------------------------

   procedure Print_Float_Array (Name          : String; anArray : Float_Array;
                                Start, Finish : Integer) is
      Count : Integer := 1;
   begin
      Put_Line (Name & ": ");
      if Start >= anArray'First and then Finish <= anArray'Last then
         for Index in Start .. Finish loop
            Put (Float'Image (anArray (Index)) & "  ");
            Count := Count + 1;
            if Count > 4 then
               New_Line;
               Count := 1;
            end if;
         end loop;
      else
         Put_Line ("Print_Float_Array called with invalid start or finish index.");
      end if;
      New_Line;
   end Print_Float_Array;

   --  ------------------------------------------------------------------------

   procedure Print_Float_List (Name : String; theList : Float_List) is
      Count : Integer := 1;
   begin
      Put_Line (Name & ": ");
      for Index in theList.First_Index .. theList.Last_Index loop
         Put (Integer'Image (Index) & ": " & Float'Image (theList.Element (Index)) &
                "   ");
         Count := Count + 1;
         if Count > 4 then
            New_Line;
            Count := 1;
         end if;
      end loop;
   end Print_Float_List;

   --  ------------------------------------------------------------------------

end Classifier_Utilities;
