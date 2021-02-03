create table if not exists 
cassandra_blacklist ( pattern varchar(255), constraint pattern_key primary key (pattern) )
engine=InnoDB default charset=utf8;

create table if not exists
cassandra_redlist ( pattern varchar(255), constraint pattern_key primary key (pattern) )
engine=InnoDB default charset=utf8;

create table if not exists
staff_journal_entries 
( uuid char(22) not null,
  written_by char(22) not null,
  written_at datetime not null,
  entry text,
  constraint uuid_key primary key (uuid),
  constraint written_by_person foreign key (written_by) references people (uuid) on delete restrict on update cascade )
engine=InnoDB default charset=utf8;

create table if not exists
staff_journal_references
( entry char(22) not null,
  person char(22) not null,
  constraint pair_key primary key (entry, person),
  constraint staff_journal_entry foreign key (entry) references staff_journal_entries (uuid) on delete restrict on update cascade,
  constraint about_whom foreign key (person) references people (uuid) on delete restrict on update cascade )
engine=InnoDB default charset=utf8;

alter table items add column if not exists effect varchar(64);

alter table items add column if not exists attributes text;

insert ignore into item_templates (id, name, default_base_color, avatar, wear_slot, description)
values (6, 'Snowball', 'FFFFFF', 'snowball', 12, 'A ball of snow that makes a playful missile.');



