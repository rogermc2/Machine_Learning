
with Ada.Containers;
with Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

with Neural_Loader;

package body Classifier_Loader is

   use ML_Types;

   function Split_Raw_Data (Raw_Data    : ML_Types.Raw_Data_Vector;
                            Num_Outputs : Positive := 1)
                            return Multi_Output_Data_Record;

   --  -------------------------------------------------------------------------

   function Load_Data (File_Name : String; Num_Outputs : Positive := 1;
                       Max_Lines : Positive := 20000)
                       return ML_Types.Multi_Output_Data_Record is
      Routine_Name : constant String := "Classifier_Loader.Load_Data ";
      Data_File    : File_Type;
      Raw_CSV_Data : ML_Types.Raw_Data_Vector;
      Output_Data  : ML_Types.Multi_Output_Data_Record;
   begin
      Put_Line (Routine_Name & "loading " & File_Name);
      Open (Data_File, In_File, File_Name);
      Raw_CSV_Data := Neural_Loader.Load_Raw_CSV_Data (Data_File, Max_Lines);
      Close (Data_File);

      Put_Line (Routine_Name & "splitting " & File_Name);

      Output_Data := Split_Raw_Data (Raw_CSV_Data, Num_Outputs);
      Put_Line (Routine_Name & File_Name & " split");

      return Output_Data;

   end Load_Data;

   --  ---------------------------------------------------------------------------

   procedure Parse_Header
     (Header       : ML_Types.Unbounded_List; Num_Features : Positive;
      Data_Record  : in out Multi_Output_Data_Record) is
   begin
      for index in 1 .. Positive (Header.Length) loop
         if index <= Num_Features then
            Data_Record.Feature_Names.Append (Header.Element (index));
         else
            Data_Record.Label_Names.Append (Header.Element (index));
         end if;
      end loop;

   end Parse_Header;

   --  -------------------------------------------------------------------------

   function Split_Raw_Data (Raw_Data    : ML_Types.Raw_Data_Vector;
                            Num_Outputs : Positive := 1)
                            return Multi_Output_Data_Record is
      use Ada.Containers;
      use Ada.Strings;
      use Ada.Strings.Unbounded;
      --        Routine_Name   : constant String :=
      --                           "Classifier_Loader.Split_Raw_Data ";
      aRow           : ML_Types.Unbounded_List := Raw_Data.First_Element;
      Num_Items      : constant Positive := Positive (aRow.Length);
      Num_Features   : constant Positive := Num_Items - Num_Outputs;
      Feature_Types  : Feature_Type_Array (1 .. Num_Features);
      Features_List  : Value_Data_Lists_2D;
      Label_Types    : Label_Type_Array  (1 .. Num_Outputs);
      Label_Values   : Value_Data_List;
      Labels_List    : Value_Data_Lists_2D;
      Feature_Values : Value_Data_List;
      Data           : Multi_Output_Data_Record;
   begin
      Parse_Header (aRow, Num_Features, Data);
      aRow := Raw_Data.Element (Positive'Succ (Raw_Data.First_Index));
      if aRow.Length > 1 then
         for f_index in 1 .. Num_Features loop
            declare
               Row_S     : constant String := To_String (aRow (f_index));
               S_Last    : constant Integer := Row_S'Last;
               Last_Char : constant Character := Row_S (S_Last);
            begin
               if Character'Pos (Last_Char) < 32 then
                  aRow (f_index) :=
                    To_Unbounded_String (Row_S (1 .. S_Last - 1));
               end if;
               Feature_Types (Positive (f_index)) :=
                 Neural_Loader.Get_Data_Type (aRow (Positive (f_index)));
            end;
         end loop;

         for l_index in 1 .. Num_Outputs loop
            declare
               Row_S     : constant String :=
                             To_String (aRow (Num_Features + l_index));
               S_Last    : constant Integer := Row_S'Last;
               Last_Char : constant Character := Row_S (S_Last);
            begin
               if Character'Pos (Last_Char) < 32 then
                  aRow (Num_Features + l_index) :=
                    To_Unbounded_String (Row_S (1 .. S_Last - 1));
               end if;
               Label_Types (Positive (l_index)) :=
                 Neural_Loader.Get_Data_Type
                   (aRow (Positive (Num_Features + l_index)));
            end;
         end loop;

         for row_index in Positive'Succ (Raw_Data.First_Index) ..
           Raw_Data.Last_Index loop
            aRow := Raw_Data.Element (row_index);  --  Unbound list

            Feature_Values.Clear;
            for f_index in 1 .. Num_Features loop
               declare
                  Feat_String : constant String := To_String (aRow (f_index));
                  Value       : Value_Record (Feature_Types (Positive (f_index)));
               begin
                  case Feature_Types (Positive (f_index)) is
                  when Boolean_Type =>
                     Value.Boolean_Value := Boolean'Value (Feat_String);
                  when Integer_Type =>
                     Value.Integer_Value := Integer'Value (Feat_String);
                  when Float_Type =>
                     Value.Float_Value := Float'Value (Feat_String);
                  when UB_String_Type =>
                     Value.UB_String_Value := aRow (f_index);
                  end case;
                  Feature_Values.Append (Value);
               end;  --  declare block
            end loop;
            Features_List.Append (Feature_Values);

            for o_index in 1 .. Num_Outputs loop
               Label_Values.Clear;
               declare
                  Row_S     : constant String :=
                                To_String (aRow (Num_Features + o_index));
                  S_Last    : constant Integer := Row_S'Last;
                  Last_Char : constant Character := Row_S (S_Last);
               begin
                  if Character'Pos (Last_Char) < 32 then
                     aRow (Num_Features + o_index) :=
                       To_Unbounded_String (Row_S (1 .. S_Last - 1));
                  end if;
               end;

               declare
                  Label       : constant String :=
                                  To_String (aRow (Num_Features + o_index));
                  Label_Value : Value_Record (Label_Types (o_index));
               begin
                  case Label_Types (Positive (o_index)) is
                  when Boolean_Type =>
                     Label_Value.Boolean_Value := Boolean'Value (Label);
                  when Integer_Type =>
                     Label_Value.Integer_Value := Integer'Value (Label);
                  when Float_Type =>
                     Label_Value.Float_Value := Float'Value (Label);
                  when UB_String_Type =>
                     Label_Value.UB_String_Value :=
                       aRow (Num_Features + o_index);
                  end case;
                  Label_Values.Append (Label_Value);
               end;  --  declare block;
               Labels_List.Append (Label_Values);
            end loop;
         end loop;

         Data.Feature_Values := Features_List;
         Data.Label_Values := Labels_List;
      end if;

      return Data;

   end Split_Raw_Data;

   --  -----------------------------------------------------------------------

end Classifier_Loader;
