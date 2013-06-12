#!/bin/bash

httrack "http://webpagedeveloper.me" "http://webpagedeveloper.me/sitemap.xml" "http://webpagedeveloper.me/namecoin" -O ./drupal_archive -%v --robots=0 
