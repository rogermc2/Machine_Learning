
with Ada.Numerics.Discrete_Random;

with Utilities;

package body Shuffler is

   generic
      type Element_Type is private;
      type Array_Type is array (Natural range <>) of Element_Type;
   procedure Generic_Shuffle (A : in out Array_Type);

   procedure Generic_Shuffle (A : in out Array_Type) is
      package Discrete_Random is new
        Ada.Numerics.Discrete_Random (Result_Subtype => Integer);
      use Discrete_Random;
      Gen       : Generator;
      New_Index : Integer;
      Item      : Element_Type;
   begin
      Reset (Gen);
      for index in reverse A'Range loop
         New_Index := (Random (Gen) mod index) + 1;
         Item := A (index);
         A (index) := A (New_Index);
         A (New_Index) := Item;
      end loop;

   end Generic_Shuffle;

   --  -------------------------------------------------------------------------

   procedure Shuffle_Integers is new Generic_Shuffle (Element_Type => Integer,
                                                      Array_Type   => Integer_Array);

   --  -------------------------------------------------------------------------

   generic
      type Element_Type is private;
      type Array_Type is array (Integer range <>, Integer range <>)
        of Element_Type;
   procedure Generic_Float_Shuffle (A : in out Array_Type);

   procedure Generic_Float_Shuffle (A : in out Array_Type) is
      package Discrete_Random is new
        Ada.Numerics.Discrete_Random (Result_Subtype => Integer);
      use Discrete_Random;
      Gen       : Generator;
      New_Index : Integer;
      Row       : array (A'Range (2)) of Element_Type;
   begin
      Reset (Gen);
      for index in reverse A'Range loop
         New_Index := (Random (Gen) mod index) + 1;
         for col in A'Range (2) loop
            Row (col) := A (index, col);
         end loop;

         for col in A'Range (2) loop
            A (index, col) := A (New_Index, col);
            A (New_Index, col) := Row (col);
         end loop;
      end loop;

   end Generic_Float_Shuffle;

   --  -------------------------------------------------------------------------

   procedure Shuffle_Floats is new
     Generic_Float_Shuffle (Element_Type => Float,
                            Array_Type   => Real_Float_Matrix);

   --  -------------------------------------------------------------------------

   procedure Shuffle (A : in out Real_Float_Matrix) is
   begin
      Shuffle_Floats (A);

   end Shuffle;

   --  -------------------------------------------------------------------------

   procedure Shuffle (A : in out Integer_Array) is
   begin
      Shuffle_Integers (A);

   end Shuffle;

   --  -------------------------------------------------------------------------

   procedure Shuffle (A : in out Real_Float_Matrix;
                      B : in out Binary_Matrix) is
      Num_Samples : constant Integer := A'Length;
      Indicies    : Integer_Array (1 .. Num_Samples);
      A2          : Real_Float_Matrix (A'Range, A'Range (2));
      B2          : Binary_Matrix (B'Range, B'Range (2));
   begin
      for row in 1 ..Num_Samples loop
         Indicies (row) := row;
      end loop;
      Utilities.Permute (Indicies);

      for row in A'Range loop
         for col in A'Range (2) loop
            A2 (row, col) := A (Indicies (row), col);
         end loop;

         for col in B'Range (2) loop
            B2 (row, col) := B (Indicies (row), col);
         end loop;
      end loop;

      A := A2;
      B := B2;

   end Shuffle;

   --  -------------------------------------------------------------------------

   procedure Shuffle (A : in out Real_Float_Matrix;
                      B : in out Integer_Array) is
      Num_Samples : constant Integer := A'Length;
      Indicies    : Integer_Array (1 .. Num_Samples);
   begin
      for row in 1 ..Num_Samples loop
         Indicies (row) := row;
      end loop;
      Utilities.Permute (Indicies);

      declare
         A2  : Real_Float_Matrix (A'Range, A'Range (2));
      begin
         for row in A'Range loop
            for col in A'Range (2) loop
               A2 (row, col) := A (Indicies (row), col);
            end loop;
         end loop;
         A := A2;
      end;

      declare
         B2  : Integer_Array (B'Range);
      begin
         for row in B'Range loop
            B2 (row) := B (Indicies (row));
            B := B2;
         end loop;
      end;

   end Shuffle;

   --  -------------------------------------------------------------------------

   procedure Shuffle (A : in out Real_Float_Matrix;
                      B : in out Integer_Matrix) is
      Num_Samples : constant Integer := A'Length;
      Indicies    : Integer_Array (1 .. Num_Samples);
      A2          : Real_Float_Matrix (A'Range, A'Range (2));
      B2          : Integer_Matrix (B'Range, B'Range (2));
   begin
      for row in 1 ..Num_Samples loop
         Indicies (row) := row;
      end loop;
      Utilities.Permute (Indicies);

      for row in A'Range loop
         for col in A'Range (2) loop
            A2 (row, col) := A (Indicies (row), col);
         end loop;

         for col in B'Range (2) loop
            B2 (row, col) := B (Indicies (row), col);
         end loop;
      end loop;

      A := A2;
      B := B2;

   end Shuffle;

   --  -------------------------------------------------------------------------

end Shuffler;
