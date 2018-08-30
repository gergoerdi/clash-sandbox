library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
use work.all;
use work.vga_types.all;

entity Top is
  port(CLK_32MHZ : in std_logic;
       RX        : in std_logic;
       TX        : out std_logic;

       -- SWITCHES  : in std_logic_array(0 to 7);
       -- JOY_UP    : in std_logic;
       -- JOY_DOWN  : in std_logic;
       -- JOY_LEFT  : in std_logic;
       -- JOY_RIGHT : in std_logic;

       LEDS      : out std_logic_vector(0 to 0);
       VGA_VSYNC : out std_logic;
       VGA_HSYNC : out std_logic;
       VGA_RED   : out unsigned(2 downto 0);
       VGA_GREEN : out unsigned(2 downto 0);
       VGA_BLUE  : out unsigned(1 downto 0)
       );
end;

architecture rtl of Top is
  signal RESET : std_logic := '0';
  signal RESET_CNT : std_logic_vector(2 downto 0) := (others => '1');

  signal CLK_25MHZ : std_logic;
  signal LED : std_logic;
begin
  process (CLK_25MHZ, RESET_CNT)
  begin
    if rising_edge(CLK_25MHZ) then
      if not (RESET_CNT = "000") then
        RESET <= '1';
        RESET_CNT <= std_logic_vector(unsigned(RESET_CNT(2 downto 0)) - 1);
      else
        RESET <= '0';
      end if;
    end if;
  end process;

  inst_clockman : entity work.clockman25
  port map (CLK_IN1 => CLK_32MHZ,
            CLK_OUT1 => CLK_25MHZ);

  inst_VGA : entity work.VGA
  port map (CLK_25MHZ => CLK_25MHZ,
            RESET => RESET,
            RX => RX,
            TX => TX,
            LED => LED,
            VGA_VSYNC => VGA_VSYNC,
            VGA_HSYNC => VGA_HSYNC,
            VGA_RED => VGA_RED,
            VGA_GREEN => VGA_GREEN,
            VGA_BLUE => VGA_BLUE);

  LEDS(0) <= LED;
end;
