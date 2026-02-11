## Process
# Select last frame of all unique plays from input tracking data
    # join with sumer sports labels for targeted defender to identify the targeted defender in each play
        # For each play in input:
            # get QB X and Y, ball_land_x, ball_land_y for last frame of the play ----> join on game_id, play_id
            # get targeted receiver X and Y, s, a, o, dir for last frame of the play ----> join on game_id, play_id
            # get targeted defender X and Y, s, a, o, dir for last frame of the play ----> join on game_id, play_id
                # keep record of targeted receiver and defender for each play to filter output data
            # join supplementary data on game_id and play_id for play-level features


# load in the output data
# filter to only include rows where targeted receiver and targeted defender match the identified targeted receiver and defender from the input data processing step
    # join with last frame of input data on game_id and play_id
    # Adjust dir and o to match unit circle convention
    # convert polar vectors to cartesian for last_frame columns ((s, dir) -> (vx, vy), (o) -> (ox, oy))
    # standardize output x, y, ball_land_x, ball_land_y, last_frame_x, last_frame_y, vx, vy, ox, oy to always be in the same direction (e.g. left to right)
    # engineer vx, vy, ax, ay for rest of output data based on differences in x, y, and time between frames



#######################################
#### Data Prep File
#######################################

# This file is used to prepare the data for the model. It will read the data from the source, clean it, and save it in a format that can be used by the model.

import polars as pl  # utilize polars efficient data handling
import os

PATH_TO_DATA = "../bdb_data/"

# checked
def load_data(file_path) -> pl.DataFrame:
    return pl.read_csv(file_path, null_values=["", "NA", "null"])

# checked
def get_file_lists(data_path) -> tuple[list, list]:
    """Get lists of input and output files from the train directory."""
    print("Getting file lists...")
    train_path = os.path.join(data_path, 'train')
    inputs = [file for file in os.listdir(train_path) if file[:5] == 'input']
    outputs = [file for file in os.listdir(train_path) if file[:6] == 'output']
    return inputs, outputs

# checked
def bind_inputs(data_path, input_files) -> pl.DataFrame:
    """Bind all input files into a single polars dataframe."""
    print("Binding input files...")
    input_dfs = [load_data(os.path.join(data_path, 'train', file)) for file in input_files]
    combined_inputs = pl.concat(input_dfs)
    return combined_inputs

# checked
def bind_outputs(data_path, output_files) -> pl.DataFrame:
    """Bind all output files into a single polars dataframe."""
    print("Binding output files...")
    output_dfs = [load_data(os.path.join(data_path, 'train', file)) for file in output_files]
    combined_outputs = pl.concat(output_dfs)
    return combined_outputs

# checked
def load_supplementary_data(data_path) -> pl.DataFrame:
    """Load supplementary data."""
    print("Loading supplementary data...")
    supp_data_path = os.path.join(data_path, 'supplementary_data.csv')
    df = load_data(supp_data_path)
    # Filter out rows where pass_result is null
    df = df.filter(pl.col("pass_result").is_not_null())

    # Convert pass_result from C/I to 0/1 integer (C=complete=0, I=incomplete=1)
    # Filter to only include the two main valid categories
    df = df.filter(
        (pl.col("pass_result") == "C") |
        (pl.col("pass_result") == "I")
    )

    df = df.with_columns([
        pl.when(pl.col("pass_result") == "C")
        .then(1)
        .when(pl.col("pass_result") == "I")
        .then(0)
        .otherwise(None)
        .cast(pl.Int32)
        .alias("pass_result")
    ])

    # Select only the columns we need and filter out any nulls
    df = df.select(["game_id", "play_id", "pass_result", "home_team_abbr", "visitor_team_abbr", "quarter", "game_clock", "down", "yards_to_go", "possession_team",
                    "defensive_team", "yardline_side", "yardline_number", "pre_snap_home_score", "pre_snap_visitor_score",
                    "pass_length", "offense_formation", "receiver_alignment", "route_of_targeted_receiver", "play_action",
                    "dropback_distance", "pass_location_type", "team_coverage_man_zone", "team_coverage_type"]).filter(pl.col("pass_result").is_not_null())

    print(f"Loaded supplementary data with {len(df)} plays")
    print(f"Pass result distribution:\n{df['pass_result'].value_counts().sort('pass_result')}")

    return df

# checked
def load_sumer_labels(data_path) -> pl.DataFrame:
    """Load sumer sports labels for targeted defender."""
    print("Loading sumer sports labels...")
    sumer_path = os.path.join(data_path, 'SumerSupplementData.csv')
    sumer_labels = load_data(sumer_path)
    return sumer_labels


def select_plays(df: pl.DataFrame) -> pl.DataFrame:
    """Select every unique play from the input tracking data and keep only the last frame of each play.
    
    This will be the building block that I join with player data on the last frame"""
    print("Selecting plays...")
    return df.sort("frame_id").group_by(["game_id", "play_id", "nfl_id"]).last()[["game_id", "play_id", "frame_id"]]


def sumer_labels_join(df: pl.DataFrame, sumer_labels: pl.DataFrame) -> pl.DataFrame:
    """Join sumer sports labels for targeted defender to main dataframe for additional features."""
    print("Joining sumer sports labels...")
    df = df.join(sumer_labels, on=['game_id', 'play_id', 'nfl_id'], how='left')
    # alter 'player_role' column so that if 'targeted_defender' == TRUE, then 'player_role' = 'targeted_defender'
    df = df.with_columns(
        pl.when(pl.col('targeted_defender') == True)
        .then(pl.lit('targeted_defender'))
        .otherwise(pl.col('player_role'))
        .alias('player_role')
    )
    return df

def select_targeted_players(df: pl.DataFrame) -> pl.DataFrame:
    """Return a dataframe for:
    - Targeted receivers: player_role == 'targeted_receiver'
    - Targeted defenders: player_role == 'targeted_defender'
    - QB: player_position == 'QB' (we will need to identify the QB for each play based on position and proximity to the ball at the start of the play)"""
    print("Selecting targeted players...")
    targeted_receivers = df.filter(pl.col('player_role') == 'Targeted Receiver')
    targeted_defenders = df.filter(pl.col('player_role') == 'targeted_defender')
    qbs = df.filter(pl.col('player_position') == 'QB')
    return targeted_receivers, targeted_defenders, qbs


def join_last_frame_with_players(last_frame_df: pl.DataFrame, targeted_receivers: pl.DataFrame, targeted_defenders: pl.DataFrame, qbs: pl.DataFrame) -> pl.DataFrame:
    """Join the last frame dataframe with the player data to get the features for the targeted receiver, defender, and QB."""
    print("Joining last frame with player data...")
    # Join last frame with targeted receivers
    df = last_frame_df.join(targeted_receivers[['game_id', 'play_id', 'frame_id', 'nfl_id', 'x', 'y', 's', 'a', 'dir', 'o']], on=['game_id', 'play_id', 'frame_id'], how='left', suffix="_receiver")
    # Join last frame with targeted defenders
    df = df.join(targeted_defenders[['game_id', 'play_id', 'frame_id', 'nfl_id', 'x', 'y', 's', 'a', 'dir', 'o']], on=['game_id', 'play_id', 'frame_id'], how='left', suffix="_defender")
    # Join last frame with QBs
    df = df.join(qbs[['game_id', 'play_id', 'frame_id', 'nfl_id', 'x', 'y', 's', 'a', 'dir', 'o', 'play_direction', 'ball_land_x', 'ball_land_y']], on=['game_id', 'play_id', 'frame_id'], how='left', suffix="_qb")
    df = df.rename({'nfl_id': 'nfl_id_receiver', 'x':'x_receiver', 'y':'y_receiver', 's':'s_receiver', 'a':'a_receiver', 'dir':'dir_receiver', 'o':'o_receiver'})
    return df

def join_with_supplementary_data(df: pl.DataFrame, supplementary_df: pl.DataFrame) -> pl.DataFrame:
    """Join the dataframe with the supplementary data to get play-level features."""
    print("Joining with supplementary data...")
    df = df.join(supplementary_df, on=['game_id', 'play_id'], how='left')
    return df

def wr_and_db_output_filter(processed_inputs: pl.DataFrame, outputs: pl.DataFrame) -> pl.DataFrame:
    """Select the nfl_id of the targeted receiver and targeted defender for each play and filter the output data to only include rows where the nfl_id of the receiver and defender match the targeted receiver and defender."""
    print("Filtering output data to only include rows where targeted receiver and defender match...")
    recievers = processed_inputs.select(['game_id', 'play_id', 'nfl_id_receiver']).unique()
    defenders = processed_inputs.select(['game_id', 'play_id', 'nfl_id_defender']).unique()
    receiver_outputs = outputs.join(recievers, left_on=['game_id', 'play_id', 'nfl_id'], right_on=['game_id', 'play_id', 'nfl_id_receiver'], how='inner')
    defender_outputs = outputs.join(defenders, left_on=['game_id', 'play_id', 'nfl_id'], right_on=['game_id', 'play_id', 'nfl_id_defender'], how='inner')
    filtered_outputs = receiver_outputs.join(defender_outputs, on=['game_id', 'play_id', 'frame_id'], how='inner', suffix="_def_live")
    return filtered_outputs

def join_model_inputs_outputs(inputs: pl.DataFrame, outputs: pl.DataFrame) -> pl.DataFrame:
    """Join the processed inputs with the filtered outputs to get the final dataframe for modeling."""
    print("Joining processed inputs with filtered outputs to get final dataframe for modeling...")
    df = outputs.join(inputs, on=['game_id', 'play_id'], how='left')
    df = df.filter(pl.col("pass_result").is_not_null())  # Filter to only include rows where pass_result is not null
    df = df.unique().sort(["game_id", "play_id", "frame_id"])
    return df

def convert_tracking_to_cartesian(tracking_df: pl.DataFrame) -> pl.DataFrame:
    """
    Convert polar coordinates to Unit-circle Cartesian format.
    We keep the original position (x, y) and kinematic variables (s, a, dir, o),
    and also compute cartesian velocity components (vx, vy) and orientation (ox, oy).

    Args:
        tracking_df (pl.DataFrame): Tracking data

    Returns:
        pl.DataFrame: Tracking data with Cartesian coordinates.
    """
    print("Converting tracking data to Cartesian coordinates...")
    return (
        tracking_df.with_columns(
            # Adjust dir and o to match unit circle convention
            dir_receiver_adjusted=((pl.col("dir_receiver") - 90) * -1) % 360,
            o_receiver_adjusted=((pl.col("o_receiver") - 90) * -1) % 360,
            dir_defender_adjusted=((pl.col("dir_defender") - 90) * -1) % 360,
            o_defender_adjusted=((pl.col("o_defender") - 90) * -1) % 360,
        )
        # convert polar vectors to cartesian ((s, dir) -> (vx, vy), (o) -> (ox, oy))
        .with_columns(
            vx_receiver_0=pl.col("s_receiver") * pl.col("dir_receiver_adjusted").radians().cos(),
            vy_receiver_0=pl.col("s_receiver") * pl.col("dir_receiver_adjusted").radians().sin(),
            ax_receiver_0=pl.col("a_receiver") * pl.col("dir_receiver_adjusted").radians().cos(),
            ay_receiver_0=pl.col("a_receiver") * pl.col("dir_receiver_adjusted").radians().sin(),
            ox_receiver_0=pl.col("o_receiver_adjusted").radians().cos(),
            oy_receiver_0=pl.col("o_receiver_adjusted").radians().sin(),

            vx_defender_0=pl.col("s_defender") * pl.col("dir_defender_adjusted").radians().cos(),
            vy_defender_0=pl.col("s_defender") * pl.col("dir_defender_adjusted").radians().sin(),
            ax_defender_0=pl.col("a_defender") * pl.col("dir_defender_adjusted").radians().cos(),
            ay_defender_0=pl.col("a_defender") * pl.col("dir_defender_adjusted").radians().sin(),
            ox_defender_0=pl.col("o_defender_adjusted").radians().cos(),
            oy_defender_0=pl.col("o_defender_adjusted").radians().sin(),
        )
        .drop(["dir_receiver_adjusted", "o_receiver_adjusted", "dir_defender_adjusted", "o_defender_adjusted"])
    )


def standardize_play_directions(df: pl.DataFrame) -> pl.DataFrame:
    """
    Standardize play directions to always moving left to right.
    Also standardize ball_land_x and ball_land_y.
    """
    print("Standardizing play directions to always move left to right...")
    return df.with_columns(
        x=pl.when(pl.col("play_direction") == "right").then(pl.col("x")).otherwise(120 - pl.col("x")),
        y=pl.when(pl.col("play_direction") == "right").then(pl.col("y")).otherwise(53.3 - pl.col("y")),
        x_def_live = pl.when(pl.col("play_direction") == "right").then(pl.col("x_def_live")).otherwise(120 - pl.col("x_def_live")),
        y_def_live = pl.when(pl.col("play_direction") == "right").then(pl.col("y_def_live")).otherwise(53.3 - pl.col("y_def_live")),
        x_receiver = pl.when(pl.col("play_direction") == "right").then(pl.col("x_receiver")).otherwise(120 - pl.col("x_receiver")),
        y_receiver = pl.when(pl.col("play_direction") == "right").then(pl.col("y_receiver")).otherwise(53.3 - pl.col("y_receiver")),
        x_defender = pl.when(pl.col("play_direction") == "right").then(pl.col("x_defender")).otherwise(120 - pl.col("x_defender")),
        y_defender = pl.when(pl.col("play_direction") == "right").then(pl.col("y_defender")).otherwise(53.3 - pl.col("y_defender")),
        x_qb = pl.when(pl.col("play_direction") == "right").then(pl.col("x_qb")).otherwise(120 - pl.col("x_qb")),
        y_qb = pl.when(pl.col("play_direction") == "right").then(pl.col("y_qb")).otherwise(53.3 - pl.col("y_qb")),
        vx_receiver_0=pl.when(pl.col("play_direction") == "right").then(pl.col("vx_receiver_0")).otherwise(-1 * pl.col("vx_receiver_0")),
        vy_receiver_0=pl.when(pl.col("play_direction") == "right").then(pl.col("vy_receiver_0")).otherwise(-1 * pl.col("vy_receiver_0")),
        ox_receiver_0=pl.when(pl.col("play_direction") == "right").then(pl.col("ox_receiver_0")).otherwise(-1 * pl.col("ox_receiver_0")),
        oy_receiver_0=pl.when(pl.col("play_direction") == "right").then(pl.col("oy_receiver_0")).otherwise(-1 * pl.col("oy_receiver_0")),
        vx_defender_0=pl.when(pl.col("play_direction") == "right").then(pl.col("vx_defender_0")).otherwise(-1 * pl.col("vx_defender_0")),
        vy_defender_0=pl.when(pl.col("play_direction") == "right").then(pl.col("vy_defender_0")).otherwise(-1 * pl.col("vy_defender_0")),
        ox_defender_0=pl.when(pl.col("play_direction") == "right").then(pl.col("ox_defender_0")).otherwise(-1 * pl.col("ox_defender_0")),
        oy_defender_0=pl.when(pl.col("play_direction") == "right").then(pl.col("oy_defender_0")).otherwise(-1 * pl.col("oy_defender_0")),
        # Also standardize the target variables
        ball_land_x=pl.when(pl.col("play_direction") == "right").then(pl.col("ball_land_x")).otherwise(120 - pl.col("ball_land_x")),
        ball_land_y=pl.when(pl.col("play_direction") == "right").then(pl.col("ball_land_y")).otherwise(53.3 - pl.col("ball_land_y")),
    ).drop("play_direction")





########################################
#### Model Prep
########################################

# the goal here is to prepare the cleaned data to be input into the model
## Inputs:
#### WR features: x, y, vx, vy (velocity components), ox, oy (orientation components)
#### DB features: x, y, vx, vy, ox, oy
#### Ball features: ball_land_x, ball_land_y
#### Play context features: yards to endzone, down, distance, game time remaining, 
####                        score differential, pass length, pass location,
####                        defenders in box, play_action, dropback distance, Coverage
#### TARGET Variable: pass_result (C = 0, I = 1)

def targets(df: pl.DataFrame) -> pl.DataFrame:
    """Select the target variable for modeling."""
    print("Selecting target variable...")
    return df.select(['game_id', 'play_id', 'frame_id', 'pass_result'])

def features(df: pl.DataFrame) -> pl.DataFrame:
    """Select the features for modeling."""
    print("Selecting features...")
    # engineer velocity and acceleration features for receiver and defender based on differences in x, y, (time between frames is 0.1s)
    df = df.sort(['game_id', 'play_id', 'frame_id']).with_columns([
        # Receiver velocity components
        ((pl.col('x').diff()).over(['game_id', 'play_id']) * 10).alias('vx_receiver'),
        ((pl.col('y').diff()).over(['game_id', 'play_id']) * 10).alias('vy_receiver'),
        # defender velocity components
        ((pl.col('x_def_live').diff()).over(['game_id', 'play_id']) * 10).alias('vx_defender'),
        ((pl.col('y_def_live').diff()).over(['game_id', 'play_id']) * 10).alias('vy_defender'),
    ])

    # fill the first row of each play with vx_receiver_0 and vx_defender_0 since the diff will be null
    df = df.with_columns([
        pl.when(pl.col('vx_receiver').is_null()).then((pl.col('x') - pl.col('x_receiver')) * 10).otherwise(pl.col('vx_receiver')).alias('vx_receiver'),
        pl.when(pl.col('vy_receiver').is_null()).then((pl.col('y') - pl.col('y_receiver')) * 10).otherwise(pl.col('vy_receiver')).alias('vy_receiver'),
        pl.when(pl.col('vx_defender').is_null()).then((pl.col('x_def_live') - pl.col('x_defender')) * 10).otherwise(pl.col('vx_defender')).alias('vx_defender'),
        pl.when(pl.col('vy_defender').is_null()).then((pl.col('y_def_live') - pl.col('y_defender')) * 10).otherwise(pl.col('vy_defender')).alias('vy_defender'),
    ])

    # compute acceleration features based on differences in velocity
    df = df.with_columns([
        ((pl.col('vx_receiver').diff()).over(['game_id', 'play_id']) * 10).alias('ax_receiver'),
        ((pl.col('vy_receiver').diff()).over(['game_id', 'play_id']) * 10).alias('ay_receiver'),
        ((pl.col('vx_defender').diff()).over(['game_id', 'play_id']) * 10).alias('ax_defender'),
        ((pl.col('vy_defender').diff()).over(['game_id', 'play_id']) * 10).alias('ay_defender'),
    ])

    # fill the first row of each play with ax_receiver_0 and ax_defender_0 since the diff will be null
    df = df.with_columns([
        pl.when(pl.col('ax_receiver').is_null()).then(pl.col('ax_receiver_0')).otherwise(pl.col('ax_receiver')).alias('ax_receiver'),
        pl.when(pl.col('ay_receiver').is_null()).then(pl.col('ay_receiver_0')).otherwise(pl.col('ay_receiver')).alias('ay_receiver'),
        pl.when(pl.col('ax_defender').is_null()).then(pl.col('ax_defender_0')).otherwise(pl.col('ax_defender')).alias('ax_defender'),
        pl.when(pl.col('ay_defender').is_null()).then(pl.col('ay_defender_0')).otherwise(pl.col('ay_defender')).alias('ay_defender'),
    ])
    # remove the intermediate velocity and acceleration columns that we used to fill in the first row of each play
    df = df.drop(['vx_receiver_0', 'vy_receiver_0', 'ax_receiver_0', 'ay_receiver_0', 
                  'vx_defender_0', 'vy_defender_0', 'ax_defender_0', 'ay_defender_0'])


    # project the ball trajectory to get ball_x, ball_y at each frame based on linear interpolation between last known ball position and ball_land_x, ball_land_y
    df = df.with_columns([
        # Calculate interpolation factor (0 at start, 1 at end of play)
        ((pl.col('frame_id') - pl.col('frame_id').min().over(['game_id', 'play_id'])) / 
        (pl.col('frame_id').max().over(['game_id', 'play_id']) - pl.col('frame_id').min().over(['game_id', 'play_id']))).alias('interp_factor')
    ]).with_columns([
        # Interpolate ball_x
        pl.when(pl.col('ball_land_x').is_not_null())
        .then(
            pl.col('x_qb') + (pl.col('ball_land_x') - pl.col('x_qb')) * pl.col('interp_factor')
        )
        .otherwise(None)
        .alias('ball_x'),
        
        # Interpolate ball_y
        pl.when(pl.col('ball_land_y').is_not_null())
        .then(
            pl.col('y_qb') + (pl.col('ball_land_y') - pl.col('y_qb')) * pl.col('interp_factor')
        )
        .otherwise(None)
        .alias('ball_y')
    ]).drop('interp_factor')

    # convert home_score and visitor_score to score differential from the perspective of the offense
    df = df.with_columns(
        pl.when(pl.col('possession_team') == pl.col('home_team_abbr'))
        .then(pl.col('pre_snap_home_score') - pl.col('pre_snap_visitor_score'))
        .otherwise(pl.col('pre_snap_visitor_score') - pl.col('pre_snap_home_score'))
        .alias('score_differential')
    )

    # create a feature for yards to endzone based on yardline_side and yardline_number (e.g. if yardline_side == possession_team, then yards_to_endzone = 100 - yardline_number, if yardline_side == 'OPP', then yards_to_endzone = yardline_number)
    df = df.with_columns(
        pl.when(pl.col('yardline_side') == pl.col('possession_team'))
        .then(100 - pl.col('yardline_number'))
        .when(pl.col('yardline_side') == pl.col('defensive_team'))
        .then(pl.col('yardline_number'))
        .otherwise(None)
        .alias('yards_to_endzone')
    )

    # create a feature for time remaining in the game based on quarter and game_clock
    df = df.with_columns([
        # Split game_clock once and reuse
        pl.col('game_clock').str.split(':').alias('_clock_parts')
    ]).with_columns([
        # Convert clock to seconds
        (pl.col('_clock_parts').list.get(0).cast(pl.Int32) * 60 + 
        pl.col('_clock_parts').list.get(1).cast(pl.Int32)).alias('_clock_seconds')
    ]).with_columns([
        # Calculate time remaining (handle overtime as 5th quarter with 10 min periods)
        pl.when(pl.col('quarter') <= 4)
        .then((4 - pl.col('quarter')) * 15 * 60 + pl.col('_clock_seconds'))
        .otherwise((5 - pl.col('quarter')) * 10 * 60 + pl.col('_clock_seconds'))  # OT periods are 10 min
        .alias('time_remaining')
    ]).drop(['_clock_parts', '_clock_seconds'])

    # create strong_side_rec and weak_side_rec based on receiver_alignment
    df = df.with_columns(
        (pl.col('receiver_alignment').str.split('x').list.get(0).cast(pl.Int32)).alias('strong_side_rec'),
        (pl.col('receiver_alignment').str.split('x').list.get(1).cast(pl.Int32)).alias('weak_side_rec')
    ).drop(['receiver_alignment'])

    # make play action binary feature
    df = df.with_columns(
        pl.when(pl.col('play_action') == True).then(1).otherwise(0).alias('play_action')
    )

    # make team_coverage_man_zone binary feature
    df = df.with_columns(
        pl.when(pl.col('team_coverage_man_zone') == 'MAN_COVERAGE').then(1).otherwise(0).alias('Man_Coverage')
    ).drop(['team_coverage_man_zone'])

    # make seperation feature based on distance between receiver and defender
    df = df.with_columns(
        ((pl.col('x') - pl.col('x_def_live'))**2 + (pl.col('y') - pl.col('y_def_live'))**2).sqrt().alias('separation')
    )

    # make distance to ball feature based on distance between receiver and ball and defender and ball
    df = df.with_columns(
        ((pl.col('x') - pl.col('ball_x'))**2 + (pl.col('y') - pl.col('ball_y'))**2).sqrt().alias('receiver_to_ball_dist'),
        ((pl.col('x_def_live') - pl.col('ball_x'))**2 + (pl.col('y_def_live') - pl.col('ball_y'))**2).sqrt().alias('defender_to_ball_dist')
    )

    # make feature for defender relative velocity (receiver velocity - defender velocity)
    df = df.with_columns(
        (pl.col('vx_receiver') - pl.col('vx_defender')).alias('relative_vx'),
        (pl.col('vy_receiver') - pl.col('vy_defender')).alias('relative_vy')
    )


    return df
    



def main():
    input_names, output_names = get_file_lists(PATH_TO_DATA) # get list of input and output files in the train directory
    input_df = bind_inputs(PATH_TO_DATA, input_names) # read in and bind all input files into a single dataframe
    output_df = bind_outputs(PATH_TO_DATA, output_names) # read in and bind all output files into a single dataframe
    supp_df = load_supplementary_data(PATH_TO_DATA) # read in supplementary data with play-level features and labels
    sumer_df = load_sumer_labels(PATH_TO_DATA) # read in sumer sports labels for targeted defender and player role
    last_plays = select_plays(input_df) # select last frame of all unique plays from input tracking data
    player_roles = sumer_labels_join(input_df, sumer_df) # join with sumer sports labels to identify player roles and targeted defenders
    receivers, defenders, qb = select_targeted_players(player_roles) # select targeted receivers, defenders, and QBs based on player roles and positions
    last_frame_context = join_last_frame_with_players(last_plays, receivers, defenders, qb) # join last frame with player data to get features for targeted receiver, defender, and QB
    last_frame_context = join_with_supplementary_data(last_frame_context, supp_df) # join with supplementary data to get play-level features
    filtered_outputs = wr_and_db_output_filter(last_frame_context, output_df) # filter output data to only include rows where targeted receiver and defender match the identified targeted receiver and defender from the input data processing step
    modeling_df = join_model_inputs_outputs(last_frame_context, filtered_outputs) # join processed inputs with filtered outputs to get final dataframe for modeling
    modeling_df = convert_tracking_to_cartesian(modeling_df) # convert polar coordinates to cartesian for model inputs
    modeling_df = standardize_play_directions(modeling_df) # standardize play directions to always be left to right
    return modeling_df


    #### Get unique plays from input data and join with sumer labels to identify targeted defender for each play



if __name__ == "__main__":
    df = main()
    print("\nData preparation complete!")
    print(f"Shape: {df.shape}")
    print(f"\nColumns: {df.columns}")
