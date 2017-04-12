SELECT total.*,
       campaigns.send_time
FROM
  (SELECT totalAct.*,
          unsubs.timestamp AS timestamp_out
   FROM
     (SELECT activity.campaign_id,
             activity.list_id,
             activity.totalActions,
             members.*
      FROM
        (SELECT email_address,
                campaign_id,
                list_id,
                count(campaign_id) AS totalActions
         FROM mailchimp_report_email_activity
         GROUP BY email_address,
                  campaign_id,
                  list_id)activity
      RIGHT JOIN
        (SELECT email_address,
                location_latitude,
                location_longitude,
                timestamp_opt
         FROM mailchimp_list_members)members ON members.email_address=activity.email_address)totalAct
   LEFT JOIN
     (SELECT TIMESTAMP,
             campaign_id,
             email_address
      FROM mailchimp_unsubscribes)unsubs ON unsubs.email_address=totalAct.email_address
   AND unsubs.campaign_id=totalAct.campaign_id)total
LEFT JOIN
  (SELECT id,
          send_time
   FROM mailchimp_campaigns) campaigns ON total.campaign_id=campaigns.id
ORDER BY email_address,
         send_time
