CREATE TABLE IF NOT EXISTS artist (
  artist_id VARCHAR(2048) PRIMARY KEY,
  artist_name VARCHAR(2048) NOT NULL
);

CREATE TABLE IF NOT EXISTS track (
  track_id VARCHAR(2048) PRIMARY KEY,
  track_name VARCHAR(2048) NOT NULL
);

CREATE TABLE IF NOT EXISTS collaboration (
  first_artist_id VARCHAR(2048) NOT NULL,
  second_artist_id VARCHAR(2048) NOT NULL,
  track_id VARCHAR(2048) NOT NULL,
  PRIMARY KEY (first_artist_id, second_artist_id, track_id),
  FOREIGN KEY (first_artist_id) REFERENCES artist (artist_id),
  FOREIGN KEY (second_artist_id) REFERENCES artist (artist_id),
  FOREIGN KEY (track_id) REFERENCES track (track_id)
);
