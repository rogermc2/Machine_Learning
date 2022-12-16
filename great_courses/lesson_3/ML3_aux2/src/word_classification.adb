
with Ada.Assertions; use Ada.Assertions;
with Ada.Containers;
with Ada.Strings.Fixed;
with Ada.Text_IO; use Ada.Text_IO;

with Neural_Utilities;

package body Word_Classification is

   type String1 is new String (1 .. 1);
   type String2 is new String (1 .. 2);

   type LC_Index is new character range 'a' .. 'z';
   One_Syllable : constant array (1 .. 9) of String1 :=
                    ("I", "A", "E", "e", "i", "x", "a", "y", "Y");
   Two_Syllable : constant array (1 .. 14) of String2 :=
                    ("ei", "iA", "iI", "ix", "AE", "Ai", "Ax", "Ix", "iE",
                     "ii", "yE", "yI", "ye", "yx");

   --  -------------------------------------------------------------------------

   procedure Build_Data (Data_In          : ML_Types.Unbounded_List;
                         Data_Out         : in out NL_Types.Boolean_List_2D;
                         Labels           : in out NL_Types.Boolean_List;
                         Words, Pronounce : in out ML_Types.Bounded_String_List) is
      use ML_Types.String_Package;
      Routine_Name : constant String := "Word_Classification.Build_Data ";
      aLine        : Unbounded_String;
      Word_Line    : ML_Types.String_List;  --  Unbounded string list
      Features     : NL_Types.Boolean_List;
   begin
      for index in Data_In.First_Index .. Data_In.Last_Index loop
         aLine := Data_In (index);
         Word_Line :=
           Neural_Utilities.Split_String_On_Spaces (To_String (aLine));
         Features := Get_Features (Word_Line);
         Data_Out.Append (Features);
         Labels.Append
           (Ada.Strings.Fixed.Index
              (To_String (Word_Line.First_Element), "ie") > 0 );
         Words.Append (To_String (Word_Line.First_Element));
         Pronounce.Append (To_String (Word_Line.Last_Element));
      end loop;

   exception
      when others =>
         Put_Line (Routine_Name & "error");
         raise;

   end Build_Data;

   --  -------------------------------------------------------------------------

   procedure Build_Dataset
     (IE_Data, EI_Data : ML_Types.Unbounded_List;
      Words, Pronounce : out ML_Types.Bounded_String_List;
      Data             : out NL_Types.Boolean_List_2D;
      Labels           : out NL_Types.Boolean_List) is

   begin
      Build_Data (IE_Data, Data, Labels, Words, Pronounce);
      Build_Data (EI_Data, Data, Labels, Words, Pronounce);

   end Build_Dataset;

   --  -------------------------------------------------------------------------

   function Feature_Names return ML_Types.Unbounded_List is
      Routine_Name : constant String := "Word_Classification.Feature_Names ";
      theLetter    : String (1 .. 1);
      Vector       : ML_Types.Unbounded_List;
   begin
      Vector.Append (To_Unbounded_String ("one syllable?"));
      Vector.Append (To_Unbounded_String ("silent?"));
      Vector.Append (To_Unbounded_String ("two syllables?"));

      --  two syllable pronunciation
      for pronounce in Two_Syllable'Range loop
         Vector.Append (To_Unbounded_String ("sounds like " &
                          String (Two_Syllable (pronounce)) & "?"));
      end loop;

      for pronounce in One_Syllable'Range loop
         Vector.Append (To_Unbounded_String ("sounds like " &
                          String (One_Syllable (pronounce)) & "?"));
      end loop;

      for letter in LC_Index loop
         theLetter (1) := Character (letter);
         --  immediate preceeding, before
         Vector.Append (To_Unbounded_String ("immediately after " &
                          theLetter & "?"));
         Vector.Append (To_Unbounded_String ("after " & theLetter & "?"));

         --  immediate following, after
         Vector.Append (To_Unbounded_String ("immediately before " &
                          theLetter & "?"));
         Vector.Append (To_Unbounded_String ("before " & theLetter & "?"));
         --  in word at all
         Vector.Append (To_Unbounded_String ("word contains " &
                          theLetter & "?"));
      end loop;

      return Vector;

   exception
      when others =>
         Put_Line (Routine_Name & "error");
         raise;
         return Vector;

   end Feature_Names;

   --  -------------------------------------------------------------------------

   --  Get_Features processes the words and extracts information about them,
   --  such as the number of sylables and pronounciation
   function Get_Features (Word_Line : ML_Types.String_List)
                          return NL_Types.Boolean_List is
      use Ada.Containers;
      use Ada.Strings.Fixed;
      use ML_Types.String_Package;
      Routine_Name : constant String := "Word_Classification.Get_Features ";
      --  aWord is wordline[0]
      aWord        : constant String := To_String (Word_Line.First_Element);
      --  Code is wordline[1]
      Code         : constant String := To_String (Word_Line.Last_Element);
      Pos          : Natural := Index (aWord, "ie");
      Minus_Char   : constant String := "-";
      Has_Letter   : Boolean;
      Two_Chars    : String2;
      Vector       : NL_Types.Boolean_List;
   begin
      Assert (Word_Line.Length = 2, Routine_Name & "invalid Word_Line length:" &
                  Count_Type'Image (Word_Line.Length));
      if Pos = 0 then
         Pos := Index (aWord, "ei");
      end if;

      Assert (Pos > 0, Routine_Name & "Word_Line has neither ie or ei.");
      --  pronounced as one syllable
      Vector.Append (Code (Pos) = '-' or Code (Pos + 1) = '-');
      --  silent
      Vector.Append (Code (Pos) = '-' and Code (Pos + 1) = '-');
      --  two syllable
      Vector.Append (Code (Pos) /= '-' and Code (Pos + 1) /= '-');
      --  Vector length 3
      --  two syllable pronunciation
      for pronounce in Two_Syllable'Range loop
         Two_Chars := String2 (Code (Pos .. Pos + 1));
         Vector.Append (Two_Chars = Two_Syllable (pronounce));
      end loop;

      --  Vector length 3 + 14 = 17
      for pronounce in One_Syllable'Range loop
         declare
            Two_Chars : constant String := Code (Pos .. Pos + 1);
         begin
            Vector.Append
              (Two_Chars = String (One_Syllable (pronounce)) & Minus_Char or
               Two_Chars = Minus_Char & String (One_Syllable (pronounce)));
         end;
      end loop;
      --  Vector length 17 + 9 = 26

      for letter in LC_Index loop
         --  immediate preceeding, before
         if Pos = 1 then
            Vector.Append (False);
         else
            Vector.Append (aWord (Pos - 1) = Character (letter));
         end if;

         Has_Letter := False;
         for let in 1 .. Pos loop
            Has_Letter := Has_Letter or aWord (let) = Character (letter);
         end loop;
         Vector.Append (Has_Letter);

         --  immediate following, after
         if Pos > aWord'Last - 2 then
            Vector.Append (False);
         else
            Vector.Append (aWord (Pos + 2) = Character (letter));
         end if;

         Has_Letter := False;
         for let in Pos + 2 .. aWord'Last loop
            Has_Letter := Has_Letter or aWord (let) = Character (letter);
         end loop;
         Vector.Append (Has_Letter);

         --  in word at all
         Has_Letter := False;
         for let in Pos + 2 .. aWord'Last loop
            Has_Letter := Has_Letter or aWord (let) = Character (letter);
         end loop;
         Vector.Append (Has_Letter);
      end loop;
      --  Vector length 26 + 5 * 26 = 156
      Assert (Vector.Length = 156, Routine_Name & "invalid ector length: " &
                  Count_Type'Image (Vector.Length) & " should be 156");
      return Vector;

   exception
      when others =>
         Put_Line (Routine_Name & "error");
         raise;
         return Vector;

   end Get_Features;

   --  -------------------------------------------------------------------------

end Word_Classification;
