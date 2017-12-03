DROP DATABASE IF EXISTS `micro_posts`;
CREATE DATABASE `micro_posts` DEFAULT CHARSET utf8 COLLATE utf8_bin;
GRANT ALL PRIVILEGES ON `micro_posts`.* TO micro_posts@localhost IDENTIFIED BY 'パスワード';