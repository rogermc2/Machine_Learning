--  Based on scikit-learn/sklearn/datasets _arff_parser.py

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
--  with Ada.Text_IO; use Ada.Text_IO;

with Load_ARFF_Data;

--  pragma Warnings (Off);

package body ARFF_Parser is

   procedure Convert_Arff_Data
     (ARFF_Container : AR_Types.ARFF_Record;
      Col_Slice_X    : IL_Types.Integer_DL_List;
      Col_Slice_Y    : IL_Types.Integer_DL_List;
      X              : out IL_Types.Float_List_2D;
      Y              : out IL_Types.Integer_List);
   function Split_Columns
     (Arff_Data       : AR_Types.AR_Real_List_2D;
      Include_Columns : IL_Types.Integer_DL_List) return IL_Types.Float_List_2D;
   function Split_Columns
     (Arff_Target    : AR_Types.AR_Integer_List;
      Include_Values : IL_Types.Integer_DL_List) return IL_Types.Integer_List;

   --  ------------------------------------------------------------------------
   --  L210
   procedure Arff_Parser
     (ARFF_Container : AR_Types.ARFF_Record;
      Features_Dict  : AR_Types.Attribute_Dictionary_Map;
      Data_Columns   : IL_Types.String_List;
      Target_Columns : IL_Types.String_List;
      Col_Slice_X    : IL_Types.Integer_DL_List;
      Col_Slice_Y    : IL_Types.Integer_DL_List;
      X              : out IL_Types.Float_List_2D;
      Y              : out IL_Types.Integer_List) is
      use AR_Types;
      use IL_Types;
      use Nominal_Data_2D_Package;
      use Nominal_Data_Package;
      use String_Package;
      --           Routine_Name      : constant String := "ARFF_Parser.Convert_Arff_Data";
      Arff_Data_Row     : AR_Types.AR_Real_List;  --  list of columns
      Data_Row          : Float_List;
      Attributes        : constant Attribute_List :=
                            ARFF_Container.Header.Attributes;
      Attribute         : Attribute_Record;
      Nominal_Data      : Nominal_Data_2D_List;
      Target_Cursor     : String_Package.Cursor := Target_Columns.First;
      Col_Name          : Unbounded_String;
      Nominal_2D_Cursor : Nominal_Data_2D_Package.Cursor;
      Nominal           : Nominal_Data_List;
      Nominal_Cursor    : Nominal_Data_Package.Cursor;
      Is_Classification : Boolean := False;
   begin
      Convert_Arff_Data (ARFF_Container, Col_Slice_X, Col_Slice_Y, X, Y);

      for v in Attributes.First_Index .. Attributes.Last_Index loop
         Attribute := Attributes.Element (v);
         if not Attribute.Nominal_Data.Is_Empty then
            Nominal_Data.Append (Attribute.Nominal_Data);
         end if;
      end loop;

      while Has_Element (Target_Cursor) loop
         Col_Name := Element (Target_Cursor);
         Nominal_2D_Cursor := Nominal_Data.First;
         while Has_Element (Nominal_2D_Cursor) loop
            Nominal := Element (Nominal_2D_Cursor);
            Nominal_Cursor := Nominal.First;
            while Has_Element (Nominal_Cursor) loop
               declare
                  Nom_Record : constant Nominal_Data_Record :=
                                 Element (Nominal_Cursor);
               begin
                  Is_Classification := Is_Classification or else
                    Nom_Record.UB_String_Data = Col_Name;
               end;
               Next  (Nominal_Cursor);
            end loop;
            Next  (Nominal_2D_Cursor);
         end loop;
         Next  (Target_Cursor);
      end loop;

      --        for row in ARFF_Container.Data.First_Index ..
      --          ARFF_Container.Data.Last_Index loop
      --           Arff_Data_Row := ARFF_Container.Data.Element (row);
      --           for index in ARFF_Container.Data.First_Index ..
      --             ARFF_Container.Data.Last_Index loop
      --              Data_Row (index) := Arff_Data_Row (index);
      --           end loop;
      --           X.Append (Data_Row);
      --           Y.Append (ARFF_Container.Target.Element (row));
      --        end loop;

   end Arff_Parser;

   --  ------------------------------------------------------------------------
   --  L151
   procedure Convert_Arff_Data
     (ARFF_Container : AR_Types.ARFF_Record;
      Col_Slice_X    : IL_Types.Integer_DL_List;
      Col_Slice_Y    : IL_Types.Integer_DL_List;
      X              : out IL_Types.Float_List_2D;
      Y              : out IL_Types.Integer_List) is
      --           Routine_Name    : constant String := "ARFF_Parser.Convert_Arff_Data";
   begin
      X := Split_Columns (ARFF_Container.Data, Col_Slice_X);
      Y := Split_Columns (ARFF_Container.Target, Col_Slice_Y);

   end Convert_Arff_Data;

   --  ------------------------------------------------------------------------
   --  L151
   --     function Convert_Arff_Data_Dataframe
   --       (ARFF_Container : ARFF.Arff_Container_Type; Features : JSON_Value)
   --        return JSON_Value is
   --        Routine_Name    : constant String :=
   --                         "ARFF_Parser.Convert_Arff_Data_Dataframe";
   --        Description     : constant JSON_Array :=
   --                            Arff_Container.Get ("description");
   --        Relation        : constant String :=
   --                            Arff_Container.Get ("relation");
   --        Attributes      : constant JSON_Array :=
   --                            Arff_Container.Get ("attributes");
   --        ARFF_Data       : constant JSON_Array :=
   --                            Arff_Container.Get ("data");
   --        First_Row       : constant JSON_Value :=
   --                            Array_Element (ARFF_Data, Array_First (ARFF_Data));
   --        Result          : JSON_Value;
   --     begin
   --        return Result;
   --
   --     end Convert_Arff_Data_Dataframe;

   --  ------------------------------------------------------------------------

   function Parse_Nominal_Data
     (Arff_Data       : AR_Types.ARFF_Record;
      Include_Columns : IL_Types.String_List)
      return AR_Types.Nominal_Data_List is
      use AR_Types;
      use IL_Types;
      use String_Package;
      use Load_ARFF_Data;
      use Nominal_Data_Package;
      --        Routine_Name  : constant String := "ARFF_Parser.Parse_Nominal_Data ";
      Attributes    : constant Attribute_List := Get_Attributes (Arff_Data);
      Include_Curs  : String_Package.Cursor := Include_Columns.First;
      Attribute     : Attribute_Record;
      Nominal_Data  : Nominal_Data_List;
      Nominal_Curs  : Nominal_Data_Package.Cursor;
   begin
      --         Put_Line (Routine_Name & "Include_Columns length: " &
      --                       Count_Type'Image (Include_Columns.Length));
      while Has_Element (Include_Curs) loop
         for Index_V in Attributes.First_Index .. Attributes.Last_Index loop
            --              Put_Line  (Routine_Name & "Index_V: " & Integer'Image (Index_V));
            Attribute := Attributes.Element (Index_V);
            Nominal_Curs := Attribute.Nominal_Data.First;
            while Has_Element (Nominal_Curs) loop
               declare
                  Nominal : constant Nominal_Data_Record :=
                              Element (Nominal_Curs);
               begin
                  Nominal_Data.Append (Nominal);
               end;
               Next  (Nominal_Curs);
            end loop;
         end loop;
         Next (Include_Curs);
      end loop;
      --         Put_Line (Routine_Name & "Nominal_Data length: " &
      --                       Count_Type'Image (Nominal_Data.Length));
      --        New_Line;

      return Nominal_Data;

   end Parse_Nominal_Data;

   --  ------------------------------------------------------------------------
   --  L18
   function Split_Columns
     (Arff_Data       : AR_Types.AR_Real_List_2D;
      Include_Columns : IL_Types.Integer_DL_List)
      return IL_Types.Float_List_2D is
      use IL_Types;
      use Integer_DLL_Package;
      --        Routine_Name  : constant String := "Openml_Ada.Split_Columns ";
      Data_New      : Float_List_2D;
      Include_Curs  : Integer_DLL_Package.Cursor;
      Arff_Data_Row : AR_Types.AR_Real_List;  --  list of columns
      New_Row       : Float_List;
   begin
      for row in Arff_Data.First_Index .. Arff_Data.Last_Index loop
         New_Row.Clear;
         Arff_Data_Row := Arff_Data.Element (row);
         Include_Curs := Include_Columns.First;
         while Has_Element (Include_Curs) loop
            New_Row.Append (Arff_Data_Row.Element (Element (Include_Curs)));
            Next (Include_Curs);
         end loop;
         Data_New.Append (New_Row);
      end loop;

      return Data_New;

   end Split_Columns;

   --  ------------------------------------------------------------------------
   --  L18
   function Split_Columns
     (Arff_Target    : AR_Types.AR_Integer_List;
      Include_Values : IL_Types.Integer_DL_List)
      return IL_Types.Integer_List is
      use IL_Types;
      use Integer_DLL_Package;
      --        Routine_Name  : constant String := "Openml_Ada.Split_Columns ";
      Data_New      : Integer_List;
      Include_Curs  : Integer_DLL_Package.Cursor;
      New_Row       : Integer_List;
   begin
      for row in Arff_Target.First_Index .. Arff_Target.Last_Index loop
         New_Row.Clear;
         Include_Curs := Include_Values.First;
         while Has_Element (Include_Curs) loop
            Data_New.Append (Arff_Target.Element (Element (Include_Curs)));
            Next  (Include_Curs);
         end loop;

      end loop;

      return Data_New;

   end Split_Columns;

   --  ------------------------------------------------------------------------

end ARFF_Parser;
