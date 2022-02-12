
with Ada.Assertions; use Ada.Assertions;
with Ada.IO_Exceptions;
with Ada.Strings.Fixed;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO;

package body Load_ARFF_Data is

   procedure Load_ARFF_Header (File_ID : File_Type;
                               Header  : out ARFF_Header_Record);

   --  ------------------------------------------------------------------------

   procedure Load_ARFF (File_Name : String; Data : out ARFF_Record) is
      Routine_Name : constant String := "Load_ARFF_Data.Load_ARFF ";
      File_ID      : File_Type;
      Header       : ARFF_Header_Record;
   begin
      Open (File_ID, In_File, File_Name);
      Load_ARFF_Header (File_ID, Header);
      Data.Header := Header;
      Close (File_ID);
      pragma Unreferenced (File_ID);

   exception
      when Ada.IO_Exceptions.Name_Error  =>
         Put_Line (Routine_Name & "can't find the file " & File_Name & "!");
         raise;
      when others =>
         Put_Line ("An exception occurred in " & Routine_Name);
         raise;

   end Load_ARFF;

   --  ------------------------------------------------------------------------

   procedure Load_ARFF_Header (File_ID : File_Type;
                               Header  : out ARFF_Header_Record) is
      use Unbounded_IO;
      use Ada.Strings;
      Routine_Name : constant String := "Load_ARFF_Data.Load_ARFF_Header ";
      aLine        : Unbounded_String;
      Is_Info      : Boolean := True;
      Pos_1        : Positive;
      Pos_2        : Positive;
      Data_Kind    : Unbounded_String;
      Attribute    : Attribute_Record;
   begin
      while Is_Info loop
         aLine := Get_Line (File_ID);
         Is_Info :=  Slice (aLine, 1, 1) = "%";
         if Is_Info then
            Header.Info.Append (Slice (aLine, 2, Length (aLine)));
         end if;
      end loop;

      while Slice (aLine, 1, 1) /= "@" loop
         Assert (Slice (aLine, 1, 9) = "@RELATION", Routine_Name &
                   "invalid ARFF format, " & To_String (aLine) & " but " &
                   " line beginning @RELATION expected");
         Header.Relation :=
           To_Unbounded_String (Slice (aLine, 11, Length (aLine)));
         aLine := Get_Line (File_ID);
      end loop;

      while Slice (aLine, 1, 1) /= "@" loop
         aLine := Get_Line (File_ID);
      end loop;

      while Slice (aLine, 1, 1) = "@" loop
         Assert (Slice (aLine, 1, 9) = "@ATTRIBUTE", Routine_Name &
                   "invalid ARFF format, " & To_String (aLine) & " but " &
                   " line beginning @ATTRIBUTE expected");
         Pos_1 := 11;
         Pos_2 := Fixed.Index (Slice (aLine, Pos_1, Length (aLine)), " ");
         Attribute.Name := Trim (To_Unbounded_String
                                 (Slice (aLine, Pos_1, Pos_2)), Both);
         Pos_1 := Pos_2 + 1;
         Data_Kind := Trim (To_Unbounded_String
                            (Slice (aLine, Pos_1, Length (aLine))), Both);
         if Data_Kind = To_Unbounded_String ("INTEGER") or
           Data_Kind = To_Unbounded_String ("REAL")
         then
            Attribute.Data_Kind := ARFF_Numeric;
         elsif Data_Kind = To_Unbounded_String ("DATE") then
            Attribute.Data_Kind := ARFF_Date;
         elsif Data_Kind = To_Unbounded_String ("STRING") then
            Attribute.Data_Kind := ARFF_String;
         elsif Slice (Data_Kind, 1, 1) = "{" then
            null;
         end if;

         Header.Attributes.Append (Attribute);
         aLine := Get_Line (File_ID);
      end loop;

   end Load_ARFF_Header;

   --  ------------------------------------------------------------------------

end Load_ARFF_Data;
