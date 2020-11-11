select
  first_artist_name,
  artist_name as second_artist_name,
  track_name
from (
  select
    artist_name as first_artist_name,
    second_artist_id,
    track_name
  from (
    select
      track_name,
      first_artist_id,
      second_artist_id
    from
      collaboration
    inner join track on collaboration.track_id=track.track_id
  ) as mid inner join artist on mid.first_artist_id=artist.artist_id
) as mid2 inner join artist on mid2.second_artist_id=artist.artist_id
where first_artist_name = 'Dirty Dike'
limit 1000;
