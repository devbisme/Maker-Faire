--*********************************************************************
-- This program is free software; you can redistribute it and/or
-- modify it under the terms of the GNU General Public License
-- as published by the Free Software Foundation; either version 2
-- of the License, or (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program; if not, write to the Free Software
-- Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
-- 02111-1307, USA.
--
-- ©2012 - X Engineering Software Systems Corp. (www.xess.com)
--*********************************************************************


--*********************************************************************
-- Circle Game.
--*********************************************************************


--*********************************************************************
-- Top-level module.
--*********************************************************************

library IEEE, UNISIM;
use IEEE.MATH_REAL.all;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;
use UNISIM.vcomponents.all;
use work.CommonPckg.all;
use work.ClkgenPckg.all;
use work.PwmPckg.all;
use work.AudioPckg.all;
use work.RotaryEncoderPckg.all;
use work.ButtonDebouncerPckg.all;
use work.LedCirclePckg.all;

entity CircleGame is
  port (
    clk_i         : in  std_logic;
    resetButton_i : in  std_logic;
    mclk_o        : out std_logic;
    sclk_o        : out std_logic;
    lrck_o        : out std_logic;
    sdti_o        : out std_logic;
    sdto_i        : in  std_logic;
    csn_o         : out std_logic;
    cclk_o        : out std_logic;
    ledCircle_o   : out std_logic_vector(9 downto 0);
    arrowLed_o    : out std_logic;
    playButton_i  : in  std_logic;
    sizeA_i       : in  std_logic;
    sizeB_i       : in  std_logic
    );
end entity;

architecture arch of CircleGame is
  constant BASE_FREQ_C : real      := 12.0;  -- Operating frequency in MHz.
  constant CLK_MUL_C   : natural   := 25;
  constant CLK_DIV_C   : natural   := 3;
  constant FREQ_C      : real      := BASE_FREQ_C * real(CLK_MUL_C) / real(CLK_DIV_C);
  signal clk_s         : std_logic;
  signal reset_s       : std_logic := YES;
  signal playButton_s  : std_logic;
  signal resetButton_s : std_logic;
  signal ledCircle_r   : unsigned(89 downto 0);
  signal arcBits_s     : unsigned(14 downto 0);
  signal pwm_r         : natural;
  signal pwm_s         : std_logic_vector(7 downto 0);
  signal doIdle_s      : boolean   := false;
  signal doPlay_s      : boolean   := false;
  signal doWin_s       : boolean   := false;
  signal doLose_s      : boolean   := false;
  signal ringReset_r   : boolean   := false;
  signal ringMove_r    : boolean   := false;
  signal arcSize_s     : std_logic_vector(6 downto 0);
  subtype audio_t is std_logic_vector(19 downto 0);
  signal audioIn_s     : audio_t;
  signal audioOut_s    : audio_t;
  signal audioXfer_s   : std_logic;
begin

  -- Generate a 100 MHz clock from the 12 MHz input clock.
  u0 : Clkgen
    generic map (BASE_FREQ_G => BASE_FREQ_C, CLK_MUL_G => CLK_MUL_C, CLK_DIV_G => CLK_DIV_C)
    port map(I               => clk_i, O => clk_s);

  process(clk_s)
    variable resetCntr_v : natural range 0 to 15 := 15;
  begin
    if rising_edge(clk_s) then
      if resetCntr_v /= 0 then
        resetCntr_v := resetCntr_v - 1;
        reset_s     <= YES;
      else
        reset_s <= NO;
      end if;
    end if;
  end process;

  u1 : ButtonDebouncer
    generic map (FREQ_G => FREQ_C)
    port map(
      clk_i    => clk_s,
      button_i => playButton_i,
      button_o => playButton_s
      );

  u2 : ButtonDebouncer
    generic map (FREQ_G => FREQ_C)
    port map(
      clk_i    => clk_s,
      button_i => resetButton_i,
      button_o => resetButton_s
      );

  u3 : LedCircleDisplay
    generic map(
      FREQ_G => FREQ_C
      )
    port map (
      clk_i        => clk_s,
      ledAll_i     => std_logic_vector(ledCircle_r),
      ledDrivers_o => ledCircle_o
      );

  process(clk_s)
    type state_t is (START, PLAY, RESULTS);
    variable state_v            : state_t;
    variable resultsTimer_v     : natural;
    constant RESULTS_INTERVAL_C : natural := 100_000_000 * 3;
  begin
    if rising_edge(clk_s) then
      doIdle_s    <= false;
      doPlay_s    <= false;
      doWin_s     <= false;
      doLose_s    <= false;
      ringReset_r <= false;
      ringMove_r  <= false;
      if reset_s = YES then
        state_v := START;
      else
        case state_v is
          when START =>
            doIdle_s    <= true;
            ringReset_r <= true;
            if playButton_s = ZERO then
              state_v := PLAY;
            end if;
          when PLAY =>
            doPlay_s   <= true;
            ringMove_r <= true;
            if playButton_s = ONE then
              resultsTimer_v := RESULTS_INTERVAL_C;
              state_v        := RESULTS;
            end if;
          when RESULTS =>
            resultsTimer_v := resultsTimer_v - 1;
            if resultsTimer_v = 0 then
              state_v := START;
            end if;
            if ledCircle_r(ledCircle_r'high) = ONE then
              doWin_s <= true;
            else
              doLose_s <= true;
            end if;
        end case;
      end if;
    end if;
  end process;

  p1 : process(clk_s)
    variable delayCnt_v : natural := 0;
    constant DELAY_C    : natural := 650_000;
  begin
    if rising_edge(clk_s) then
      if ringReset_r then
        ledCircle_r                  <= (others => ZERO);
        ledCircle_r(arcBits_s'range) <= arcBits_s;
        delayCnt_v                   := 0;
      elsif ringMove_r then
        if delayCnt_v < DELAY_C then
          delayCnt_v := delayCnt_v + 1;
        else
          delayCnt_v  := 0;
          ledCircle_r <= ledCircle_r rol 1;
        end if;
      else
        null;
      end if;
    end if;
  end process;

  p2 : process(clk_s)
    variable delayCnt_v  : natural := 0;
    variable delayStep_v : natural := 1;
    constant DELAY_C     : natural := 200_000;
    variable pwmDelta_v  : natural;
    constant IDLE_MAX_C  : natural := 255;
    constant IDLE_MIN_C  : natural := 0;
  begin
    if rising_edge(clk_s) then
      if reset_s = YES then
        delayCnt_v := 0;
        pwm_r      <= IDLE_MIN_C + 1;
        pwmDelta_v := 1;
      else
        if delayCnt_v < DELAY_C then
          delayCnt_v := delayCnt_v + delayStep_v;
        else
          delayCnt_v                      := 0;
          if pwm_r >= IDLE_MAX_C or pwm_r <= IDLE_MIN_C then
            pwmDelta_v := -pwmDelta_v;
          end if;
          pwm_r <= pwm_r + pwmDelta_v;
          if doIdle_s then
            delayStep_v := 1;
          elsif doWin_s then
            delayStep_v := 10;
          elsif doLose_s then
            delayStep_v := 5;
          else
            null;
          end if;
        end if;
      end if;
    end if;
  end process;

  pwm_s <= std_logic_vector(TO_UNSIGNED(pwm_r, pwm_s'length));

  u4 : Pwm
    port map(
      clk_i  => clk_s,
      duty_i => pwm_s,
      pwm_o  => arrowLed_o
      );

  u5 : RotaryEncoderWithCounter
    generic map(
      INITIAL_CNT_G    => 12,
      ALLOW_ROLLOVER_G => false
      )
    port map(
      clk_i   => clk_s,
      reset_i => reset_s,
      a_i     => sizeA_i,
      b_i     => sizeB_i,
      cnt_o   => arcSize_s
      );

  p3 : process(arcSize_s)
  begin
    case TO_INTEGER(unsigned(arcSize_s(arcSize_s'high downto 2))) is
      when 0 | 1  => arcBits_s <= "000000000000001";
      when 2      => arcBits_s <= "000000000000011";
      when 3      => arcBits_s <= "000000000000111";
      when 4      => arcBits_s <= "000000000001111";
      when 5      => arcBits_s <= "000000000011111";
      when 6      => arcBits_s <= "000000000111111";
      when 7      => arcBits_s <= "000000001111111";
      when 8      => arcBits_s <= "000000011111111";
      when 9      => arcBits_s <= "000000111111111";
      when 10     => arcBits_s <= "000001111111111";
      when 11     => arcBits_s <= "000011111111111";
      when 12     => arcBits_s <= "000111111111111";
      when 13     => arcBits_s <= "001111111111111";
      when 14     => arcBits_s <= "011111111111111";
      when others => arcBits_s <= "111111111111111";
    end case;
  end process;

  u6 : Audio
    generic map(
      FREQ_G => FREQ_C
      )
    port map(
      rst_i      => reset_s,
      clk_i      => clk_s,
      leftAdc_o  => open,
      rightAdc_o => open,
      leftDac_i  => audioOut_s,
      rightDac_i => audioOut_s,
      xfer_o     => audioXfer_s,
      mclk_o     => mclk_o,
      sclk_o     => sclk_o,
      lrck_o     => lrck_o,
      sdti_o     => sdti_o,
      sdto_i     => sdto_i,
      csn_o      => csn_o,
      cclk_o     => cclk_o
      );

  WaveFsm : process(clk_s)
    type wave_t is array(0 to 15) of integer;
    variable wave_v         : wave_t                := (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
--    variable wave_v : wave_t := (0, 1000, 2000, 3000, 4000, 5000, 6000, 7000, 8000,
--    7000, 6000, 5000, 4000, 3000, 2000, 1000); 
--    0, -1000, -2000, -3000, -4000, -5000, -6000, -7000, -8000, 
--    -7000, -6000, -5000, -4000, -3000, -2000, -1000);
    variable address_v      : unsigned(31 downto 0);
    variable addressDelta_v : unsigned(31 downto 0) := TO_UNSIGNED(8, 32);
  begin
    if rising_edge(clk_s) then
      if audioXfer_s = YES then
        audioOut_s <= audio_t(TO_SIGNED(wave_v(TO_INTEGER(address_v(31 downto 16))), audioOut_s'length));
        address_v  := address_v + addressDelta_v;
      end if;
    end if;
  end process;

  -- AudioFsm : process(clk_s)
  -- begin
  -- if doIdle_s then
  -- elsif doPlay_s then
  -- elsif doWin_s then
  -- elsif doLose_s then
  -- else
  -- end if;
  -- end process;

end architecture;


