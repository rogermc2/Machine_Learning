
with Ada.Containers.Ordered_Sets;
with Ada.Text_IO; use Ada.Text_IO;

with Encoder;

package body Classifier_Utilities is

   package Int_Sets is new Ada.Containers.Ordered_Sets (Integer);

   --  -------------------------------------------------------------------------

   procedure Clear (anArray : in out Label_Data_Array) is
   begin
      for index in anArray'Range loop
         anArray (index) := 0.0;
      end loop;
   end Clear;

   --  -------------------------------------------------------------------------
   --  Compute_Class_Weight estimates class weights for unbalanced datasets.
   function Compute_Class_Weight (Class_Weight : Weight_Type;
                                  Y            : Integer_Array_List;
                                  Classes      : Integer_List;
                                  Weights      : Weight_List :=
                                    Weight_Package.Empty_Vector)
                                  return Float_List is
      Weight : Float_List;
      LE     : Encoder.Label_Encoder;
      Y_Ind  : Integer_List;
   begin
      if Class_Weight = No_Weight then
         for index in Classes.First_Index .. Classes.Last_Index loop
            Weight.Append (1.0);
         end loop;
      elsif Class_Weight = Balanced_Weight then
         --           Y_Ind := Encoder.Fit_Transform (LE, Y);
         null;
      else  --  user-defined dictionary
         null;
      end if;
      return Weight;
   end Compute_Class_Weight;

   --  -------------------------------------------------------------------------

   function Compute_Sample_Weight (Class_Weight : Weight_Type;
                                   Y            : Integer_Array_List;
                                   Indices      : Integer_List :=
                                     Integer_Package.Empty_Vector;
                                   Weights      : Weight_List :=
                                     Weight_Package.Empty_Vector)
                                   return Float_List is
      use Integer_Package;
      Y_Curs                : Integer_Array_Package.Cursor := Y.First;
      Expanded_Class_Weight : Weight_Map;
      Sample_Weights        : Weight_Map;
      Num_Outputs           : Integer := Y.Last_Index;
      Y_Array               : array (1 .. Num_Outputs) of
        Integer_Array (1 .. Y.First_Element'Length);
      Y_Full                : Integer_Array (1 .. Y_Array (1)'Length);
      Class_Weight_K        : Float;
      --        Weight_K              : Float;
      Sample_Weight         : Float_List;
   begin
      --        if Class_Weight /= Balanced_Weight then
      --           raise Value_Error with
      --             "Compute_Sample_Weight; Weight does not contain the only" &
      --             " valid preset for class_weight which is balanced.";
      --        end if;

      for index in Y_Array'Range loop
         Y_Array (Index) := Y.Element (index);
      end loop;

      for index in Y_Array'Range loop
         for index_2 in Y_Array (index) (1) .. Y_Array (index)'Last loop
            Y_Full (index_2) := Y_Array (index) (index_2);
         end loop;
         declare
            Classes_Full : Integer_Array := Unique_Array Y_Full);
         begin
            if Class_Weight = Balanced_Weight or Num_Outputs = 1 then
               Class_Weight_K := 1.0;
               --                 Class_Weight_K := Class_Weight;
            elsif Class_Weight = List_Of_Weights then
               Class_Weight_K := Weights.Element (index).Weight;
            end if;
         end;

         if Indices.Is_Empty then
            null;
         else
            null;
         end if;
      end loop;

      for index in Y.First_Index .. Num_Outputs loop
         Y_Full := Y_Array (index);
      end loop;

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

   function Unique_Integer (Nums : ML_Types.Label_Data_Array)
                            return Integer_List is
      use Int_Sets;
      use Integer_Package;
      Unique_Set : Int_Sets.Set;
--        Int_Curs   : Integer_Package.Cursor := Nums.First;
      Set_Curs   : Int_Sets.Cursor;
      Nums_List  : Integer_List;
   begin
      for index in Nums'First .. Nums'Last loop
         Unique_Set.Include (Nums (index).Integer_Value);
      end loop;

      Set_Curs := Unique_Set.First;
      while Has_Element (Set_Curs) loop
         Nums_List.Append (Element (Set_Curs));
         Next (Set_Curs);
      end loop;
      return Nums_List;
   end Unique_Integer;

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
