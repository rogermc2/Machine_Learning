
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
--  with Ada.Text_IO; use Ada.Text_IO;

package body ARFF is

   package String_Package is new Ada.Containers.Doubly_Linked_Lists (Unbounded_String);
   subtype String_List is String_Package.List;

   type Arff_Decoder is record
         Current_Line : Integer := 0;
   end record;

function Decode (Decoder : in Out Arff_Decoder; File_Data : String; Encode_Nominal : Boolean := False;
                  Return_Type : ARFF_Return_Type := Arff_Dense)
                 return JSON_Value is
      use Ada.Strings;
      Data_Length         : constant Integer := File_Data'Length;
      Pos1                : Integer := 1;
      Pos2                : Integer := 1;
      Message_Lines       : String_List;
      Arff_Container_Type : JSON_Value := Create_Object;
      ARFF_Data           : JSON_Value;
      Attribute_Names     : JSON_Value := Create_Object;
   begin
      Decoder.Current_Line := 0;
      while Pos2 /= 0 and Pos1 < Data_Length loop
         Pos2 := Fixed.Index (File_Data, "\r\n");
         Message_Lines.Append (To_Unbounded_String (File_Data (Pos1 .. Pos2)));
         Pos1 := Pos2 + 4;
      end loop;
      if Pos1 < Data_Length - 4 then
         Message_Lines.Append
           (To_Unbounded_String (File_Data (Pos1 .. Data_Length - 4)));
      end if;

      Arff_Container_Type.Set_Field ("description", "");
      Arff_Container_Type.Set_Field ("relation", "");
      Arff_Container_Type.Set_Field ("attributes", Create (Empty_Array));
      Arff_Container_Type.Set_Field ("description", Create (Empty_Array));

      return ARFF_Data;

   end Decode;

   --  -------------------------------------------------------------------------

   function Load (File_Data : String; Encode_Nominal : Boolean := False;
                  Return_Type : ARFF_Return_Type := Arff_Dense)
                  return JSON_Value is
     Decoder : Arff_Decoder;
      ARFF_Data : constant JSON_Value :=
                    Decode (Decoder, File_Data, Encode_Nominal, Return_Type);
   begin
      return ARFF_Data;

   end Load;

   --  -------------------------------------------------------------------------

end ARFF;
