
with Ada.Numerics.Discrete_Random;

package body Shuffler is

   package Discrete_Random is new
     Ada.Numerics.Discrete_Random (Result_Subtype => Integer);

   --  -------------------------------------------------------------------------

   generic
      type Element_Type is private;
      type Array_Type is array (Natural range <>) of Element_Type;
   procedure Generic_Shuffle (A : in out Array_Type);

   procedure Generic_Shuffle (A : in out Array_Type) is
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

   generic
      type Element_Type1 is private;
      type Element_Type2 is private;
      type Array_Type1 is array (Integer range <>, Integer range <>)
        of Element_Type1;
      type Array_Type2 is array (Natural range <>) of Element_Type2;
   procedure Generic_MA_Shuffle (A : in out Array_Type1;
                                 B : in out Array_Type2);

   procedure Generic_MA_Shuffle (A : in out Array_Type1;
                                 B : in out Array_Type2) is
      use Discrete_Random;
      Gen       : Generator;
      New_Index : Integer;
      Row1      : array (A'Range (2)) of Element_Type1;
      Value     : Element_Type2;
   begin
      Reset (Gen);
      for index in reverse A'Range loop
         New_Index := (Random (Gen) mod index) + 1;
         for col in A'Range (2) loop
            Row1 (col) := A (index, col);
         end loop;

         for col in A'Range (2) loop
            A (New_Index, col) := A (index, col);
            A (index, col) := Row1 (col);
         end loop;

         Value := B (index);
         B (index) := B (New_Index);
         B (New_Index) := Value;
      end loop;

   end Generic_MA_Shuffle;

   --  -------------------------------------------------------------------------

   procedure Shuffle_MA is new
     Generic_MA_Shuffle (Element_Type1 => Float, Element_Type2 => Integer,
                         Array_Type1   => Real_Float_Matrix,
                         Array_Type2   => Integer_Array);

   --  -------------------------------------------------------------------------

   generic
      type Element_Type1 is private;
      type Element_Type2 is private;
      type Array_Type1 is array (Integer range <>, Integer range <>)
        of Element_Type1;
      type Array_Type2 is array (Integer range <>, Integer range <>)
        of Element_Type2;
   procedure Generic_MM_Shuffle (A : in out Array_Type1;
                                 B : in out Array_Type2);

   procedure Generic_MM_Shuffle (A : in out Array_Type1;
                                     B : in out Array_Type2) is
      use Discrete_Random;
      Gen       : Generator;
      New_Index : Integer;
      Row1      : array (A'Range (2)) of Element_Type1;
      Row2      : array (B'Range (2)) of Element_Type2;
   begin
      Reset (Gen);
      for index in reverse A'Range loop
         New_Index := (Random (Gen) mod index) + 1;
         for col in A'Range (2) loop
            Row1 (col) := A (index, col);
            Row2 (col) := B (index, col);
         end loop;

         for col in A'Range (2) loop
            A (index, col) := A (New_Index, col);
            A (New_Index, col) := Row1 (col);
         end loop;

         for col in B'Range (2) loop
            B (index, col) := B (New_Index, col);
            B (New_Index, col) := Row2 (col);
         end loop;
      end loop;

   end Generic_MM_Shuffle;

   --  -------------------------------------------------------------------------

   procedure Shuffle_FI is new
     Generic_MM_Shuffle (Element_Type1 => Float, Element_Type2 => Integer,
                            Array_Type1   => Real_Float_Matrix,
                            Array_Type2 => Integer_Matrix);

   --  -------------------------------------------------------------------------

   procedure Shuffle_FB is new
     Generic_MM_Shuffle (Element_Type1 => Float, Element_Type2 => Binary,
                            Array_Type1   => Real_Float_Matrix,
                            Array_Type2 => Binary_Matrix);

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
   begin
     Shuffle_FB (A, B);

   end Shuffle;

   --  -------------------------------------------------------------------------

   procedure Shuffle (A : in out Real_Float_Matrix;
                      B : in out Integer_Array) is
   begin
      Shuffle_MA (A, B);

   end Shuffle;

   --  -------------------------------------------------------------------------

   procedure Shuffle (A : in out Real_Float_Matrix;
                      B : in out Integer_Matrix) is
   begin
      Shuffle_FI (A, B);

   end Shuffle;

   --  -------------------------------------------------------------------------

end Shuffler;