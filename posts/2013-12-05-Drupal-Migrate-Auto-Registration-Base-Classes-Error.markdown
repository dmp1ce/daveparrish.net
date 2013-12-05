---
title: Drupal Migrate error due to auto registration and base classes
date: 2013-12-05 14:47:47
---

If you are getting an error similar to the following when using the `drush ms` command:

```
[vagrant@localhost drupal]$ drush ms
PHP Fatal error:  Call to a member function importedCount() on a non-object in /vagrant/drupal/profiles/commerce_kickstart/modules/contrib/migrate/includes/migration.inc on line 847
Drush command terminated abnormally due to an unrecoverable error.                  [error]
Error: Call to a member function importedCount() on a non-object in
/vagrant/drupal/profiles/commerce_kickstart/modules/contrib/migrate/includes/migration.inc,
line 847
```

Then you may have accidently had a base migration class registered when you ran `drush mar`.

To recover from this, you need to deregister the base classes in your project.  In my case the fix was the following:

```
$ drush migrate-deregister ItemCommon
$ drush migrate-deregister OrderCommon
```
