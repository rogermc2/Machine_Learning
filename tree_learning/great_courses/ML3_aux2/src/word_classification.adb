
with Ada.Strings.Fixed;
--  with Ada.Text_IO;

with Utilities;

with NL_Types;

package body Word_Classification is

   type String1 is new String (1 .. 1);
   type String2 is new String (1 .. 2);

   type LC_Index is new character range 'a' .. 'z';
   One_Syllable : constant array (1 .. 9) of String1 :=
                    ("I", "A", "E", "e", "i", "x", "a", "y", "Y");
   Two_Syllable : constant array (1 .. 14) of String2 :=
                    ("ei", "iA", "iI", "ix", "AE", "Ai", "Ax", "Ix", "iE",
                     "ii", "yE", "yI", "ye", "yx");

   function Get_Features (Word_Line : ML_Types.String_List)
                          return NL_Types.Boolean_List;

   --  -------------------------------------------------------------------------

   procedure Build_Dataset (IE_Data, EI_Data : ML_Types.Unbounded_List) is
      use ML_Types.String_Package;
      Curs      : Cursor;
      aLine     : Unbounded_String;
      Word_Line : ML_Types.String_List;
      Features  : NL_Types.Boolean_List;
      Labels    : ML_Types.Unbounded_List;
      Words     : ML_Types.Unbounded_List;
      Pronounce : ML_Types.Unbounded_List;
      Data      : NL_Types.Boolean_List_2D;
   begin
      for index in IE_Data.First_Index .. IE_Data.Last_Index loop
         aLine := IE_Data (index);
         Word_Line := Utilities.Split_String_On_Spaces (To_String (aLine));
         Features := Get_Features (Word_Line);
         Data.Append (Features);
         Curs := Word_Line.First;
         Words.Append (Element (Curs));
         Pronounce.Append (Element (Next (Curs)));
         declare
            aWord : constant String := To_String (Words.Last_Element);
         begin
            if Ada.Strings.Fixed.Index (aWord, "ie") > 0 then
               Labels.Append (To_Unbounded_String (aWord));
            end if;
         end;

         aLine := EI_Data (index);
         Word_Line := Utilities.Split_String_On_Spaces (To_String (aLine));
      end loop;

   end Build_Dataset;

   --  -------------------------------------------------------------------------

   function Feature_Names return ML_Types.Unbounded_List is
      theLetter : String (1 .. 1);
      Vector    : ML_Types.Unbounded_List;
   begin
      Vector.Append (To_Unbounded_String ("one syllable?"));
      Vector.Append (To_Unbounded_String ("silent?"));
      Vector.Append (To_Unbounded_String ("two syllables?"));

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

   end Feature_Names;

   --  -------------------------------------------------------------------------

   --  Get_Features processes the words and extracts information about them,
   --  such as the number of sylables and pronounciation
   function Get_Features (Word_Line : ML_Types.String_List)
                          return NL_Types.Boolean_List is
      use Ada.Strings.Fixed;
      use ML_Types.String_Package;
      Curs         : constant Cursor := Word_Line.First;
      aWord        : constant String := To_String (Element (Curs));
      Code         : constant String := To_String (Element (Next (Curs)));
      Pos          : Natural;
      Minus_Char   : constant String := "-";
      Two_Chars    : String2;
      Vector       : NL_Types.Boolean_List;
   begin
      Pos := Index (aWord, "ie");
      --  pronounced as one syllable
      Vector.Append (Code (Pos) = '-' or Code (Pos + 1) = '-');
      --  silent
      Vector.Append (Code (Pos) = '-' and Code (Pos + 1) = '-');
      --  two syllable
      Vector.Append (Code (Pos) /= '-' and Code (Pos + 1) /= '-');

      for pronounce in Two_Syllable'Range loop
         Two_Chars := String2 (Code (Pos ..Pos + 1));
         Vector.Append (Two_Chars = Two_Syllable (pronounce));
      end loop;

      for pronounce in One_Syllable'Range loop
         declare
            Two_Chars : constant String := Code (Pos ..Pos + 1);
         begin
            Vector.Append
              (Two_Chars = String (One_Syllable (pronounce)) & Minus_Char);
         end;
      end loop;

      for letter in LC_Index loop
         --  immediate preceeding, before
         if Pos = 1 then
            Vector.Append (False);
         else
            Vector.Append (aWord (Pos - 1) = Character (letter));
         end if;

         --  immediate following, after
         if Pos > aWord'Last - 2 then
            Vector.Append (False);
         else
            Vector.Append (aWord (Pos + 2) = Character (letter));
            for index in Pos + 2 .. aWord'Last loop
               Vector.Append (aWord (index) = Character (letter));
            end loop;
            --  in word at all
            for index in Pos + 2 .. aWord'Last loop
               Vector.Append (aWord (index) = Character (letter));
            end loop;
         end if;
      end loop;

      return Vector;

   end Get_Features;

   --  -------------------------------------------------------------------------

end Word_Classification;
